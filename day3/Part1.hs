module Part1 where

newtype Counter = Counter (Int, Int)

data Bit = Zero | One deriving (Eq, Show)
type Binary = [Bit]
type Report = [Binary]

readBit :: Char -> Bit
readBit '0' = Zero
readBit '1' = One
readBit _   = error "Unexpected character"

readBinary :: String -> Binary
readBinary = map readBit

updateCount :: Counter -> Bit -> Counter
updateCount (Counter (zeroes, ones)) b = case b of
  Zero -> Counter (zeroes + 1, ones)
  One -> Counter (zeroes, ones + 1)

updateCounts :: [Counter] -> Binary -> [Counter]
updateCounts = zipWith updateCount

mostCommon :: [Binary] -> Binary
mostCommon bs =
  let
    len = length $ head bs
    initialCounts = replicate len (Counter (0, 0))
    finalCounts = foldl updateCounts initialCounts bs
  in map (\(Counter (zeroes, ones)) -> if zeroes > ones then Zero else One) finalCounts

binaryToInt :: Binary -> Int
binaryToInt = foldl f 0
  where
    f acc Zero = 2 * acc
    f acc One = 2 * acc + 1

gammaRate :: Binary -> Int
gammaRate = binaryToInt

flipBit :: Bit -> Bit
flipBit Zero = One
flipBit One = Zero

epsilonRate :: Binary -> Int
epsilonRate = binaryToInt . map flipBit

powerConsumption :: Report -> Int
powerConsumption report =
  let m = mostCommon report
  in gammaRate m * epsilonRate m

parseReport :: String -> Report
parseReport = map readBinary . lines

readReport :: FilePath -> IO Report
readReport filePath = fmap parseReport $ readFile filePath
-- readReport filePath = do
--   input <- readFile filePath
--   return $ map readBinary $ lines input

main = do
  report <- readReport "input.txt"
  print $ powerConsumption report
