module Day12.Pathing where

import Data.Char (isAlpha, isLower, isUpper)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import Util

type Cave = String

type CaveSystem = Map Cave [Cave]

type Path = [Cave]

inputP :: Parser CaveSystem
inputP = M.fromListWith (<>) . concat <$> many1 edgeP
  where
    edgeP = tokenize $ do
      start <- many1 $ satisfy isAlpha
      char '-'
      end <- many1 $ satisfy isAlpha
      return [(start, [end]), (end, [start])]

paths :: Part -> CaveSystem -> [Path]
paths part m = paths' [["start"]]
  where
    cond :: Part -> Cave -> Path -> Bool
    cond _ "start" _ = False
    cond part c p =
      length small_caves - length (S.fromList small_caves) <= difference part
      where
        small_caves = filter (all isLower) (c : p)
        difference Part1 = 0
        difference Part2 = 1

    paths' :: [Path] -> [Path]
    paths' ps = do
      p@(c : _) <- ps
      next <- m M.! c
      if next == "end"
        then return $ "end" : p
        else do
          guard $ all isUpper next || cond part next p
          paths' [next : p]

pathing :: Part -> CaveSystem -> Int
pathing part = length . paths part
