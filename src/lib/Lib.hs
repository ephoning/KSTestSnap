{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib
    ( loadAndProcess
    ) where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Internal as BS
import Data.Aeson
import Data.List
import GHC.Generics
import Debug.Trace
import Text.Printf

data KeywordRecord = KeywordRecord { keyword :: String
                                   , max_cpc_latest_prediction :: Double
                                   } deriving Generic
instance FromJSON KeywordRecord

predictionExtractor = max_cpc_latest_prediction

type Coord = (Double,Double)
type Index = Int

-- (KS statistic (Dnn') , cAlpha root , null hypothesis is rejected)
type Stat = (Double, Double, Bool)

alpha = 0.025

loadAndProcess :: String -> String -> IO String
loadAndProcess samplesFilePathA samplesFilePathB = do
  inputA <- B.readFile samplesFilePathA
  inputB <- B.readFile samplesFilePathB
  let inputsA = B.split (BS.c2w '\n') inputA
  let inputsB = B.split (BS.c2w '\n') inputB
  let stats = process [inputsA,inputsB]
  return stats
  
process :: [[B.ByteString]] -> String
process bss = do
  let samples = byteStrings2CFCoords bss
  let ((dist, cAlphaRoot, nullHRejected), distCoords) = calcStats samples alpha
  let formattedCAlphaRoot = printf "%.4f" cAlphaRoot :: String
  let stats = "2-sample KS statistic:  dist = " ++ show dist ++ " / cAlphaRoot = " ++ formattedCAlphaRoot ++ " / null Hyp. rejected = " ++ show nullHRejected
  stats
  
-- convert byte string lists (each list representing contents of a single file) to corresponding Coord lists
byteStrings2CFCoords :: [[B.ByteString]] -> [[Coord]]
byteStrings2CFCoords = map byteString2CFCoords
  where byteString2CFCoords :: [B.ByteString] -> [Coord]
        byteString2CFCoords bs = doubles2CFCoords $ extract_predictions bs

-- convert a byte string list (representing contents of a file) to a list with the prediction values from those byte strings)
extract_predictions :: [B.ByteString] -> [Double]
extract_predictions bss = extract_preds bss []
  where extract_preds :: [B.ByteString] -> [Double] -> [Double]
        extract_preds [] accu = accu
        extract_preds (bs:bss) accu = do
          let mk = decode bs :: Maybe KeywordRecord
          case mk of
            Nothing -> extract_preds bss accu
            Just m -> extract_preds bss ((predictionExtractor m) : accu)

doubles2CFCoords :: [Double] -> [Coord]
doubles2CFCoords [] = []
doubles2CFCoords ds = ds2cfps (group $ sort ds) 0 (toInteger (length ds))
  where ds2cfps :: [[Double]] -> Integer -> Integer -> [Coord]
        ds2cfps [] _ _ = []
        ds2cfps (ds:dss) count totalLen = ((head ds, cdf (cumCount count ds) totalLen ) :: Coord) : (ds2cfps dss (cumCount count ds) totalLen)
        cumCount ::  Integer -> [Double] -> Integer -- Cumulative Count
        cumCount c ds = toInteger (length ds) + c
        cdf :: Integer -> Integer -> Double -- Cumulative Distribution Fraction
        cdf cnt tot = (fromInteger cnt) / (fromInteger tot)


------------------------------------------------------------------------------------------------------------------------
--- see: https://en.wikipedia.org/wiki/Kolmogorov%E2%80%93Smirnov_test ---

calcStats :: [[Coord]] -> Double -> (Stat, [Coord])
calcStats samples alpha =
  let sampleA = samples !! 0
      sampleB = samples !! 1
      candidates = pairUpCandidates sampleA sampleB
      sortedCandidates = reverse $ sortOn ( \ ((_,y),(_,y')) -> abs y - y') candidates
      topCandidate = head sortedCandidates
      ksDist = abs (snd $ fst topCandidate) - (snd $ snd topCandidate)
      caRoot = cAlphaRoot sampleA sampleB alpha
      stat = (ksDist, (cAlphaRoot sampleA sampleB alpha), ksDist > caRoot)
  in (stat, [fst topCandidate, snd topCandidate])

-- cAlphaRoot: 
cAlphaRoot :: [Coord] -> [Coord] -> Double -> Double
cAlphaRoot cs1 cs2 alpha = (cAlpha alpha) * sqrt (fromIntegral (n + n') / fromIntegral  (n * n'))
  where n = length cs1
        n' = length cs2

-- cAlpha:
cAlpha :: Double -> Double
cAlpha a = sqrt (-(0.5) * log (a / 2.0))

------------
-- pairCandidates: pair up Coord instances of cs1 and cs2 where each instance from cs1 is paired with the cs2 instance that has the closest domain value
-- (i.e., the 2 instances line up vertically as much as possible in their plot representations )
pairUpCandidates :: [Coord] -> [Coord] -> [(Coord,Coord)]
pairUpCandidates cs1 cs2 =
  let extremes = findExtremes cs1 cs2
      cs1' = bracket cs1 extremes
      cs2' = bracket cs2 extremes
  in map ( \ c -> pairUpCandidate c cs2') cs1'

-- findExtrmeses: find the extremes between which both ranges have domain values
findExtremes :: [Coord] -> [Coord] -> (Double,Double)
findExtremes cs1 cs2 = (max (fst $ head cs1) (fst $ head cs2), min (fst $ last cs1) (fst $ last cs2))

-- bracket: truncate/bracket Coord instances on the basis of provided extremes
bracket :: [Coord] -> (Double,Double) -> [Coord]
bracket cs (left,right) = filter (\ (x,_) -> x >= left && x <= right) cs

-- pair up a candidate coord with the Coord element from the list that has the closest domain value (i.e, in the first position in the Coord pair)
pairUpCandidate :: Coord -> [Coord] -> (Coord,Coord)
pairUpCandidate c@(cx,_) cs = triple2Pair $ head $ sortOn (\ (_,_,z) -> z) $ map (\ c'@(cx',_) -> (c, c', abs $ cx - cx')) cs


------------------------------------------------------------------------------------------------------------------------
--- generic utilities

triple2Pair :: (a,b,c) -> (a,b)
triple2Pair (a,b,_) = (a,b)

