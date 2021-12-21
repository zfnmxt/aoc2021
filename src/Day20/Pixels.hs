module Day20.Pixels where

import qualified Data.List as L
import qualified Data.Map as M
import Util hiding (look, neighbors)

type Algorithm = String

type Pos = (Int, Int)

type Image = Map Pos Char

inputP :: Parser (Algorithm, Image)
inputP = do
  algorithm <- many $ satisfy (`elem` ".#")
  char '\n'
  ws
  rows <- zip [0 ..] <$> sepBy (zip [0 ..] <$> many (satisfy (`elem` ".#"))) (char '\n')
  ws
  let image = M.fromList $ concatMap (\(y, r) -> map (\(x, p) -> ((y, x), p)) r) rows
  return (algorithm, image)

stencil :: (Int, Int) -> [(Int, Int)]
stencil (y, x) =
  map (\(x, y) -> (y, x)) $
    [(x - 1, y - 1), (x, y - 1), (x + 1, y - 1)]
      ++ [(x - 1, y), (x, y), (x + 1, y)]
      ++ [(x - 1, y + 1), (x, y + 1), (x + 1, y + 1)]

look :: Image -> Pos -> Char
look m pos =
  case m M.!? pos of
    Just c -> c
    Nothing -> m M.! head (M.keys m)

pixToBit :: Char -> Bit
pixToBit '.' = '0'
pixToBit '#' = '1'

grow :: Image -> Image
grow im = im `M.union` new_pixels
  where
    (x_min, y_min) = head $ M.keys im
    (x_max, y_max) = last $ M.keys im
    x_min' = x_min - 1
    x_max' = x_max + 1
    y_min' = y_min - 1
    y_max' = y_max + 1
    new_pixels =
      M.fromList $
        map (,'.') $ do
          x <- [x_min' .. x_max']
          y <- [y_min' .. y_max']
          return (x, y)

stencilToInt :: Image -> [(Int, Int)] -> Int
stencilToInt m = bin2int . map (pixToBit . look m)

algoOnPixel :: Algorithm -> Image -> (Int, Int) -> Char
algoOnPixel alg im p = alg !! idx
  where
    idx = stencilToInt im $ stencil p

step :: Algorithm -> Image -> Image
step alg im = M.mapWithKey (\pos _ -> algoOnPixel alg im pos) $ grow im

stepN :: Int -> Algorithm -> Image -> Image
stepN n alg = iterateN n (step alg)

printImage :: Image -> String
printImage im = unlines $ (map . map) snd $ L.groupBy (\l r -> fst (fst l) == fst (fst r)) lim
  where
    lim = M.toList im
    (x, y) = maximum $ M.keys im

pixels :: Part -> Algorithm -> Image -> Int
pixels part alg = length . M.filter (== '#') . stepN (if part == Part1 then 2 else 50) alg . grow
