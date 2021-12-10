module Day02.Dive where

import Data.Char (isSpace)

type Position = (Int, Int)

type State = (Position, Int)

data Command = Forward Int | Down Int | Up Int

parse :: String -> Command
parse s =
  case com of
    "forward" -> Forward n
    "down" -> Down n
    "up" -> Up n
  where
    (com, rest) = break isSpace s
    n = read rest

move :: Position -> Command -> Position
move (x, y) (Forward dx) = (x + dx, y)
move (x, y) (Down dy) = (x, y + dy)
move (x, y) (Up dy) = (x, y - dy)

move2 :: State -> Command -> State
move2 ((x, y), a) (Forward dx) = ((x + dx, y + a * dx), a)
move2 ((x, y), a) (Down da) = ((x, y), a + da)
move2 ((x, y), a) (Up da) = ((x, y), a - da)

main :: IO ()
main = do
  cmds <- map parse . lines <$> getContents
  let (x1, y1) = foldl move (0, 0) cmds
  putStrLn $ "part1 (x * y): " <> show (x1 * y1)
  let ((x2, y2), _) = foldl move2 ((0, 0), 0) cmds
  putStrLn $ "part2 (x * y): " <> show (x2 * y2)
