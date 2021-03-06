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
import Control.Monad       (forever, when)
import Control.Monad.Trans (lift)
import Control.Pipe
import Data.Attoparsec.ByteString as A hiding (take)
import Data.ByteString     (ByteString)  
import Data.Maybe          (fromMaybe)
import Data.Map.Strict     (Map)
import qualified Data.Map.Strict as Map
import Data.Time
import Data.Time.Clock.POSIX
import Network.Pcap 
import Options.Applicative hiding (Parser) 
import qualified Options.Applicative as Options
import System.Exit         (exitSuccess)

default (ByteString, Int)

-- | Parse command line arguments and run the pcap parser accordingly.
main :: IO ()
main = execParser opts >>= entry
  where opts = info (helper <*> options)
          ( fullDesc
          <> progDesc "Parse a pcap file according to spec."
          <> header   "A Kospi Quote Parser"                 )

-- Argument parsing ------------------------------------------------------------

-- | Options consist of the target pcap dump file and the optional reorder flag.
data Options = Options { reorder :: Bool, dumpFile :: FilePath }

-- | An options parser.
options :: Options.Parser Options
options = Options <$> switch (  short 'r' 
                             <> long  "reorder" 
                             <> help  "Reorder quotes by accept time." )
                  <*> argument str (metavar "PCAPFILE")

-- | Enter the program's IO pipeline.
entry :: Options -> IO ()
entry (Options r d) = do
    d0 <- openOffline d 
    runPipe $     yieldPackets d0 
              >+> extractQuotes 
              >+> if r then sortingBuffer >+> printer else printer

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
    case A.parse (quote (hdrUTCTime hdr)) payload of
        Fail {}   -> return ()
        Done _ r  -> yield  (Just r)
        Partial _ -> error $    "failed to filter quote packets (pcap stream "
                             ++ "has likely ended or been corrupted)"

-- | Await quotes and hold them in a 3-second buffer.  If upstream yields a
--   Nothing, flush the buffer and exit gracefully.
sortingBuffer :: Pipe (Maybe Quote) (Maybe Quote) IO ()
sortingBuffer = go Map.empty where
    go buffer = await >>= \maybeQ -> case maybeQ of
      Nothing -> flush buffer
      Just q  -> let buffer0 = Map.insert (hashTimes q) q buffer
                     (minq, buffer1) = bufferMin buffer0
                     (maxq, _      ) = bufferMax buffer0
                 in if   abs (pktTime maxq `diffUTCTime` acceptTime minq) > 3
                    then yield (Just minq) >> go buffer1
                    else                      go buffer0

-- | Flush a buffer.
flush :: Monad m => Map k a -> Pipe b (Maybe a) m ()
flush b | Map.null b = yield Nothing 
        | otherwise  = (\(m, r) -> yield (Just m) >> flush r) (bufferMin b)

-- | Await Maybes and print Justs to stdout.  If a Nothing is received, exit the
--   program gracefully.
printer :: Show a => Consumer (Maybe a) IO b
printer = forever $ await >>= \x -> case x of 
      Nothing -> lift exitSuccess
      Just q  -> (lift . print) q

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
 
-- | Convert a packet header's timestamp to UTC.
hdrUTCTime :: PktHdr -> UTCTime
hdrUTCTime = posixSecondsToUTCTime . realToFrac . hdrDiffTime 

-- | Create a unique key for packet/accept times. 
hashTimes :: Quote -> String
hashTimes q = show (utcToInteger (acceptTime q)) 
           ++ show (utcToInteger (pktTime q))

-- | Convert a UTC time to Integer (required to avoid overflow on 32-bit 
--   systems).
utcToInteger :: UTCTime -> Integer
utcToInteger t = truncate $ utcTimeToPOSIXSeconds t * 10^(6 :: Int) 

