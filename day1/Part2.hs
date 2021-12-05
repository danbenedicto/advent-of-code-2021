module Part2 where

import Part1 (countIncreases)

windows :: [a] -> [(a, a, a)]
windows xs = zip3 xs (tail xs) (tail $ tail xs)

main = do
  report <- readFile "input.txt"
  let
    depths = map read $ lines report :: [Int]
    windowSums = map (\(x, y, z) -> x + y + z) $ windows depths
  print $ countIncreases windowSums
