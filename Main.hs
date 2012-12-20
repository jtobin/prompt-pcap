-- |
-- Module      :  Main
-- Copyright   :  Jared Tobin 2012
-- License     :  BSD3
--
-- Maintainer  :  jared@jtobin.ca
-- Stability   :  experimental
-- Portability :  unknown

{-# OPTIONS_GHC -Wall #-}

module Main where

import Kospi
import Control.Monad
import Control.Monad.Trans
import Control.Pipe
import Data.Attoparsec.ByteString    as A       hiding (take)
import Data.ByteString               as B       hiding (take)
import Data.Maybe
import Data.Map.Strict    (Map)
import qualified Data.Map.Strict     as Map
import Data.Time
import Data.Time.Clock.POSIX
import Network.Pcap
import Options.Applicative                      hiding (Parser) 
import qualified Options.Applicative as Options
import System.Exit

default (ByteString, Int)

-- | Parse command line arguments and run the pcap parser accordingly.
main :: IO ()
main = execParser opts >>= entry
  where opts = info (helper <*> options)
          ( fullDesc
          & progDesc "Parse a pcap file according to spec."
          & header   "A Kospi Quote Parser"                 )

-- Argument parsing ------------------------------------------------------------

-- | Options consist of the target pcap dump file and the optional reorder flag.
data Options = Options { reorder :: Bool, dumpFile :: FilePath }

-- | An options parser.
options :: Options.Parser Options
options = Options <$> switch ( short 'r' 
                             & long  "reorder" 
                             & help  "Reorder quotes by accept time." )
                  <*> argument str (metavar "PCAPFILE" )
-- | Enter into the main program.  
entry :: Options -> IO ()
entry (Options r d) = do
    let finalizer = if   r 
                    then bufferAndSort >+> printer 
                    else printOrTerminate
    d0 <- openOffline d 
    runPipe $ yieldPackets d0 >+> extractQuotes >+> finalizer

-- IO pipeline -----------------------------------------------------------------

-- | Yield the contents of a handle, terminating upon reaching an empty packet.
--   Note that this should also work for live captures, though that's untested.
yieldPackets :: PcapHandle -> Producer (PktHdr, ByteString) IO b
yieldPackets handle = forever $ lift (nextBS handle) >>= yield

-- | Yield only quote packets, according to spec.  
extractQuotes :: Monad m => Pipe (PktHdr, ByteString) (Maybe Quote) m b
extractQuotes = forever $ do
    (hdr, payload) <- await
    when (hdrCaptureLength hdr == 0) $ yield Nothing
    let hdrUTCTime = posixSecondsToUTCTime . realToFrac . hdrDiffTime $ hdr
    case A.parse (quote hdrUTCTime) payload of 
        Fail {}   -> return ()
        Done _ r  -> yield  (Just r)
        Partial _ -> error $    "failed to filter quote packets (pcap stream " 
                             ++ "has likely ended or been corrupted)"

-- | Await quotes and hold them in a 3-second buffer.  If upstream yields a 
--   Nothing, pass control to a terminating pipe.  The utcToInt helper 
--   must have type Integer to avoid overflow on 32-bit systems.
bufferAndSort :: Pipe (Maybe Quote) Quote IO ()
bufferAndSort = go Map.empty where
    go buffer = await >>= \maybeQ -> 
      if   isNothing maybeQ
      then flushAndTerminate buffer
      else let q = fromMaybe (error buffError) maybeQ
               buffer0 = Map.insert (hashTimes q) q buffer
               (minq, buffer1) = bufferMin buffer0
               (maxq, _)       = bufferMax buffer0
           in if   abs (pktTime maxq `diffUTCTime` acceptTime minq) > 3 
              then yield minq >> go buffer1 
              else               go buffer0
    hashTimes q = show (utcToInt (acceptTime q))
               ++ show (utcToInt (pktTime q))
    utcToInt t  = truncate $ utcTimeToPOSIXSeconds t * 10^(6 :: Int) :: Integer

-- | Flush the Quote buffer and exit gracefully when it's empty.
flushAndTerminate :: Map k b -> Pipe a b IO r
flushAndTerminate b 
    | Map.null b = lift exitSuccess 
    | otherwise  = (\(m, r) -> yield m >> flushAndTerminate r) (bufferMin b)

-- | Print anything received to stdout.  Used to print Quotes that have been
--   unwrapped from 'Just's upstream.
printer :: Show a => Consumer a IO b
printer = forever $ await >>= lift . print

-- | Unwrap Just Quotes and print them to stdout.  If a Nothing is received,
--   exit the program gracefully.
printOrTerminate :: Show a => Consumer (Maybe a) IO b
printOrTerminate = forever $ do
    x <- await 
    when (isNothing x) (lift exitSuccess)
    (lift . print) (fromMaybe (error "pipeline expected a 'Just' wrapper") x)

-- Utilities -------------------------------------------------------------------

-- | The minimum element of a buffer.
bufferMin :: Map k a -> (a, Map k a)
bufferMin b = fromMaybe (error buffError) (Map.minView b)

-- | The maximum element of a buffer.
bufferMax :: Map k a -> (a, Map k a)
bufferMax b = fromMaybe (error buffError) (Map.maxView b)

-- | Standard error to throw if a buffer behaves unexpectedly.
buffError :: String
buffError =    "failed to buffer quote packets (pcap stream has likely been "
            ++ "corrupted)"
                                    
