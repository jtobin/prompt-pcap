-- |
-- Module      :  Kospi.Parser
-- Copyright   :  Jared Tobin 2012
-- License     :  BSD3
--
-- Maintainer  :  jared@jtobin.ca
-- Stability   :  experimental
-- Portability :  unknown

{-# OPTIONS_GHC -Wall -Werror -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}

module Kospi.Parser
    ( -- * Parsers
      quote
    ) where

import Kospi.Data
import Control.Applicative                         hiding (empty)
import Control.Monad
import Data.Attoparsec.Binary                
import qualified Data.Attoparsec.ByteString as A          (take)
import Data.Attoparsec.ByteString           as A   hiding (take)
import Data.ByteString                      as B   hiding (take)
import qualified Data.ByteString.Char8      as B8 
import Data.Maybe                 
import Data.Time
import Data.Word

default (ByteString, Int)

-- | A quote packet parser.
quote :: UTCTime -> Parser Quote
quote ptime = ethernetFrame *> ipHeader *> udpHeader *> quotePayload ptime

-- | A parser for the payload of a quote packet.  The parser is passed the 
--   timestamp from the packet header for inclusion in the resulting Quote.  
--   Intermediate bytes between items of interest are discarded.
--
--   We are interested in quotes beginning with the ASCII bytes B6034.
quotePayload :: UTCTime -> Parser Quote
quotePayload ptime = do 
    string "B6034"
    icode                <- liftM B8.unpack isinCode
    throwAway 12
    [b1, b2, b3, b4, b5] <- replicateM 5 quotePQ
    throwAway 7
    [a1, a2, a3, a4, a5] <- replicateM 5 quotePQ
    throwAway 50
    atime                <- liftM (rawTimeToUTC ptime) rawTime
    throwAway 1
    return $ Quote ptime atime icode 
                   b5 b4 b3 b2 b1 
                   a1 a2 a3 a4 a5
  where 
    rawTimeToUTC base offset = 
        localTimeToUTC (hoursToTimeZone 9) (LocalTime (utctDay base) offset) 

-- | A 14-byte Ethernet frame parser. 
ethernetFrame :: Parser ByteString
ethernetFrame = A.take 14

-- | A 20-byte IPv4 header parser. 
ipHeader :: Parser ByteString
ipHeader = A.take 20

-- | A UDP header parser.  We are interested in packets with destination ports 
--   15515/15516.
udpHeader :: Parser Word16
udpHeader = A.take 2 *> (word16be 15515 <|> word16be 15516) <* A.take 4

-- | A 12-byte security identification code parser.
isinCode :: Parser ByteString
isinCode = A.take 12

-- | A parser for the price and quantity of a bid/ask quote.
quotePQ :: Parser PQ
quotePQ = PQ <$> nInts 5 <*> nInts 7

-- | Consume n Ints.
nInts :: Int -> Parser Int
nInts n = asInt <$> A.take n
  where asInt = fst . fromMaybe (error msg) . B8.readInt
        msg   = "failed to parse bytes to Int (pcap stream has likely been "
             ++ "corrupted)"

-- | Consume a 'raw' time having format HHMMSSuu.  Note that 'uu' here
--   represents centiseconds.
rawTime :: Parser TimeOfDay
rawTime = do [hh, mm, ss, uu] <- replicateM 4 (nInts 2)
             return $ TimeOfDay hh mm (fromIntegral ss + fromIntegral uu / 100)

-- | Throw away bytes.  This is just an alias for A.take.
throwAway :: Int -> Parser ByteString
throwAway = A.take

