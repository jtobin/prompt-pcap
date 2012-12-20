-- |
-- Module      :  Kospi.Data
-- Copyright   :  Jared Tobin 2012
-- License     :  BSD3
--
-- Maintainer  :  jared@jtobin.ca
-- Stability   :  experimental
-- Portability :  unknown

{-# OPTIONS_GHC -Wall #-}

module Kospi.Data 
    ( -- * Data types
      Quote(..)
    , PQ(..)
    ) where

import Data.ByteString                 as B hiding (take)
import qualified Data.ByteString.Char8 as B8
import Data.Function (on)
import Data.Time
import System.Locale

-- | A Quote containing only the information we're interested in.
data Quote = Quote { pktTime    :: {-# UNPACK #-} !UTCTime
                   , acceptTime :: {-# UNPACK #-} !UTCTime
                   , issueCode  :: {-# UNPACK #-} !ByteString
                   , bid5       :: {-# UNPACK #-} !PQ 
                   , bid4       :: {-# UNPACK #-} !PQ 
                   , bid3       :: {-# UNPACK #-} !PQ 
                   , bid2       :: {-# UNPACK #-} !PQ 
                   , bid1       :: {-# UNPACK #-} !PQ 
                   , ask1       :: {-# UNPACK #-} !PQ
                   , ask2       :: {-# UNPACK #-} !PQ
                   , ask3       :: {-# UNPACK #-} !PQ
                   , ask4       :: {-# UNPACK #-} !PQ
                   , ask5       :: {-# UNPACK #-} !PQ         } deriving Eq

-- | Rank Quotes by accept time at the exchange.
instance Ord Quote where 
    q0 `compare` q1 = let byTime = compare `on` acceptTime 
                      in  q0 `byTime` q1

-- | Show Quotes according to spec.
instance Show Quote where
    show q =           showTimeToPrecision 4 (pktTime q)
             ++ " " ++ showTimeToPrecision 2 (acceptTime q)
             ++ " " ++ B8.unpack (issueCode q)
             ++ " " ++ show (bid5 q)
             ++ " " ++ show (bid4 q)
             ++ " " ++ show (bid3 q)
             ++ " " ++ show (bid2 q)
             ++ " " ++ show (bid1 q)
             ++ " " ++ show (ask1 q)
             ++ " " ++ show (ask2 q)
             ++ " " ++ show (ask3 q)
             ++ " " ++ show (ask4 q)
             ++ " " ++ show (ask5 q)

-- | Price/quantity information for a given quote.
data PQ = PQ {-# UNPACK #-} !Int {-# UNPACK #-} !Int deriving Eq

-- | Show price/quantity information according to spec.
instance Show PQ where show (PQ a b) = show b ++ "@" ++ show a 

-- | Pretty-print timestamps according to a custom spec.  
showTimeToPrecision :: FormatTime t => Int -> t -> String
showTimeToPrecision n t =    formatTime defaultTimeLocale "%F %H:%M:%S" t 
                          ++ "." ++ ms ++ " " ++ tz
    where ms = take n $ formatTime defaultTimeLocale "%q" t
          tz = formatTime defaultTimeLocale "%Z" t

