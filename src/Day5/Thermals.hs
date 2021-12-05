module Day5.Thermals where

import Data.Array (Array)
import qualified Data.Array as A
import Data.Map (Map)
import qualified Data.Map as M
import Util

type Point = (Int, Int)

type Line = (Point, Point)

type Diagram = Array (Int, Int) Int

inputP :: Parser [Line]
inputP = many $ do
  x1 <- int
  char' ','
  y1 <- int
  string' "->"
  x2 <- int
  char' ','
  y2 <- int
  return ((x1, y1), (x2, y2))

range :: Int -> Int -> [Int]
range start end
  | start <= end = [start .. end]
  | otherwise = reverse [end .. start]

toPoints :: Part -> Line -> [Point]
toPoints part ((x1, y1), (x2, y2))
  | x1 == x2 = [(x1, y) | y <- range y1 y2]
  | y1 == y2 = [(x, y1) | x <- range x1 x2]
  | otherwise =
    case part of
      Part1 -> mempty
      Part2 -> zip (range x1 x2) (range y1 y2)

diagramBounds :: [Point] -> (Int, Int)
diagramBounds = foldr (\(x, y) (x', y') -> (max x x', max y y')) (minBound, minBound)

toDiagram :: Part -> [Line] -> Diagram
toDiagram part ls = A.accumArray (+) 0 bounds $ zip ps (repeat 1)
  where
    ps = concatMap (toPoints part) ls
    bounds = ((0, 0), diagramBounds ps)

numOverlaps :: Diagram -> Int
numOverlaps = sum . fmap (\x -> if x > 1 then 1 else 0)

thermals :: Part -> [Line] -> Int
thermals part = numOverlaps . toDiagram part
