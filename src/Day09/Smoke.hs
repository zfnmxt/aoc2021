module Day09.Smoke where

import Data.Array (Array, (!))
import qualified Data.Array as A
import Data.Ix (inRange)
import Data.List (product, sort)
import Util

type Pos = (Int, Int)

type Cave = Array Pos Int

type Basin = [Pos]

type Bound = (Pos, Pos)

inputP :: Parser Cave
inputP = do
  rs@(r : _) <- tokenize $ sepBy (many1 digit) ws1
  let bounds = ((0, 0), (length rs - 1, length r - 1))
  return $ A.listArray bounds $ concat rs

lowPoint :: Cave -> Pos -> Bool
lowPoint c p = all ((> (c ! p)) . (c !)) $ neighbors c p

lowPoints :: Cave -> [Pos]
lowPoints c = filter (lowPoint c) (A.range $ A.bounds c)

risk :: Int -> Int
risk = (+ 1)

basin :: Cave -> Pos -> Basin
basin c (x, y) = map fst $ filter (fst . snd) $ A.assocs $ dfs (fmap (False,) c) [(x, y)]
  where
    dfs :: Array Pos (Bool, Int) -> [Pos] -> Array Pos (Bool, Int)
    dfs mc [] = mc
    dfs mc (p : ps) = dfs (mc A.// [(p, (True, c ! p))]) (todo ++ ps)
      where
        todo = filter (\n -> notVisited n && notLess n && notNine n) $ neighbors c p
        notVisited n = not $ fst $ mc ! n
        notLess n = c ! n >= c ! p
        notNine n = c ! n /= 9

smoke :: Part -> Cave -> Int
smoke Part1 c = sum $ map (risk . (c !)) $ lowPoints c
smoke Part2 c = product $ take 3 $ reverse $ sort basin_sizes
  where
    basins = map (basin c) $ lowPoints c
    basin_sizes = map length basins
