module Day6.Lanternfish where

import Data.List (splitAt)
import Data.Map (Map)
import qualified Data.Map as M
import Util

type Lanternfish = [Int]

initialize :: [Int] -> Lanternfish
initialize fish =
  M.elems $
    M.fromListWith (+) (zip fish (repeat 1)) `M.union` M.fromList (zip [0 ..] $ replicate 9 0)

step :: Lanternfish -> Lanternfish
step fs = f16 ++ [f0 + f7, f8, f0]
  where
    (f0 : f16, [f7, f8]) = splitAt 7 fs

inputP :: Parser [Int]
inputP = sepBy int (char' ',')

lanternfish :: Part -> [Int] -> Int
lanternfish Part1 fs = sum $ iterate step (initialize fs) !! 80
lanternfish Part2 fs = sum $ iterate step (initialize fs) !! 256
