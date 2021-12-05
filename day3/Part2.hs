module Part2 where

import Data.Function (on)
import Part1 (Report, Binary, mostCommon, binaryToInt, readReport, flipBit)

matchesAt :: (Eq a) => Int -> [a] -> [a] -> Bool
matchesAt index = (==) `on` (!! index)
-- matchesAt index xs ys = xs !! index == ys !! index

oxygenGeneratorRating :: Report -> Int
oxygenGeneratorRating report =
  let
    narrow :: [Binary] -> Int -> Binary
    narrow [b] _ = b
    narrow bs n  =
      let matches = filter (matchesAt n (mostCommon bs)) bs
      in narrow matches (n + 1)
  in binaryToInt $ narrow report 0

leastCommon :: [Binary] -> Binary
leastCommon = map flipBit . mostCommon

co2ScrubberRating :: Report -> Int
co2ScrubberRating report =
  let
    narrow :: [Binary] -> Int -> Binary
    narrow [b] _ = b
    narrow bs n  =
      let matches = filter (matchesAt n (leastCommon bs)) bs
      in narrow matches (n + 1)
  in binaryToInt $ narrow report 0

lifeSupportRating :: Report -> Int
lifeSupportRating report =
  oxygenGeneratorRating report * co2ScrubberRating report

main = do
  report <- readReport "input.txt"
  print $ lifeSupportRating report
