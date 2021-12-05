module Part2 where

import Part1 (Command(..), parseCourse)

newtype State = State (Int, Int, Int)

applyCommand :: State -> Command -> State
applyCommand (State (h, depth, aim)) command =
  case command of
    (Forward x) -> State (h + x, depth + aim * x, aim)
    (Up x) -> State (h, depth, aim - x)
    (Down x) -> State (h, depth, aim + x)

main = do
  course <- parseCourse "input.txt"
  let
    finalPos = foldl applyCommand (State (0, 0, 0)) course
    (State (h, depth, _)) = finalPos
  print (h * depth)
