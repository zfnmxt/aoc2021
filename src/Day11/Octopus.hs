module Day11.Octopus where

import qualified Data.Array as A
import Data.Bifunctor (bimap, first, second)
import qualified Data.List as L
import Util

type Octopus = (Int, Int)

type Pos = (Int, Int)

type Octopuses = Array Pos Octopus

energy :: Octopus -> Int
energy = fst

num :: Octopus -> Int
num = snd

modifyEnergy :: (Int -> Int) -> Octopus -> Octopus
modifyEnergy = first

modifyNum :: (Int -> Int) -> Octopus -> Octopus
modifyNum = second

inputP :: Parser Octopuses
inputP = do
  os <- tokenize $ sepBy1 (many1 digit) ws1
  let bounds = ((0, 0), (length os - 1, length (head os) - 1))
  return $ fmap (,0) $ A.listArray bounds $ concat os

adjacent :: Octopuses -> Pos -> [Pos]
adjacent os (x, y) =
  filter
    (\(x, y) -> x >= x_min && x <= x_max && y >= y_min && y <= y_max)
    ( [(x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y)]
        ++ [(x - 1, y - 1), (x - 1, y + 1), (x + 1, y - 1), (x + 1, y + 1)]
    )
  where
    ((x_min, y_min), (x_max, y_max)) = A.bounds os

step :: Octopuses -> Octopuses
step octos =
  fmap (modifyEnergy (\x -> if x > 9 then 0 else x)) $
    (\os -> flash os [p | (p, (x, _f)) <- A.assocs os, x > 9]) $
      fmap (modifyEnergy (+ 1)) octos
  where
    flash :: Octopuses -> [Pos] -> Octopuses
    flash os [] = os
    flash os (p : ps)
      | energy (os ! p) > 9 && num (os ! p) == num (octos ! p) =
        let os' = os // [(p, modifyNum (+ 1) (os ! p))]
            adj = adjacent os' p
            os'' = os' // [(p', modifyEnergy (+ 1) (os' ! p')) | p' <- adj]
         in flash os'' (adj ++ ps)
      | otherwise = flash os ps

octopus :: Part -> Octopuses -> Int
octopus Part1 os = sum $ num <$> iterateN 100 step os
octopus Part2 os = snd $ until (foldr (\x b -> energy x == 0 && b) True . fst) (bimap step (+ 1)) (os, 0)
