module Day7.Crabs where

import Util

type Position = Int

inputP :: Parser [Position]
inputP = sepBy int (char' ',')

fuel :: Part -> [Position] -> Position -> Int
fuel Part1 ps to = sum $ map (\p -> abs (p - to)) ps
fuel Part2 ps to = sum $ map (\p -> let n = abs (p - to) in n * (n + 1) `div` 2) ps

crabs :: Part -> [Position] -> Int
crabs part ps = minimum $ map (fuel part ps) [minimum ps .. maximum ps]
