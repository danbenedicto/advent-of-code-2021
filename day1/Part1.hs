module Part1 where

countIf :: (a -> Bool) -> [a] -> Int
countIf pred = length . filter pred

countIncreases :: (Ord a) => [a] -> Int
countIncreases xs = countIf (\(x, y) -> x < y) $ zip xs (tail xs)

main = do
  report <- readFile "input.txt"
  let depths = map read $ lines report :: [Int]
  print $ countIncreases depths
