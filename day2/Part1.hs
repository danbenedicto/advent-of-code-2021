module Part1 where

newtype Loc = Loc (Int, Int)

data Command = Forward Int | Up Int | Down Int

applyCommand :: Loc -> Command -> Loc
applyCommand (Loc (h, depth)) command =
  case command of
    (Forward x) -> Loc (h + x, depth)
    (Up x) -> Loc (h, depth - x)
    (Down x) -> Loc (h, depth + x)

parseCommand :: String -> Command
parseCommand line = case words line of
  ["forward", n] -> Forward (readInt n)
  ["up", n] -> Up (readInt n)
  ["down", n] -> Down (readInt n)
  _ -> error "Unexpected command"

parseCourse :: FilePath -> IO [Command]
parseCourse filePath = do
  courseRaw <- readFile filePath
  return $ map parseCommand $ lines courseRaw

readInt :: String -> Int
readInt = read

main = do
  course <- parseCourse "input.txt"
  let
    finalPos = foldl applyCommand (Loc (0, 0)) course
    (Loc (h, depth)) = finalPos
  print (h * depth)
