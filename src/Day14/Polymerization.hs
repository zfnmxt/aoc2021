module Day14.Polymerization where

import Data.Char
import Data.List (sort, sortOn)
import qualified Data.Map.Strict as M
import Util

type Polymer = String

type Rules = Map (Char, Char) Char

type Counts = Map (Char, Char) Integer

inputP :: Parser (Rules, Polymer)
inputP = do
  template <- many $ satisfy isLetter
  ws
  rules <- sepBy ruleP ws
  return (M.fromList rules, template)
  where
    ruleP = do
      l <- satisfy isLetter
      r <- satisfy isLetter
      ws
      string' "->"
      gen <- satisfy isLetter
      return ((l, r), gen)

step :: Rules -> Counts -> Counts
step rs cs = M.fromListWith (+) $ do
  (pair@(l, r), n) <- M.toList cs
  let g = rs M.! pair
  [((l, g), n), ((g, r), n)]

synthesize :: Rules -> Polymer -> Int -> Counts
synthesize rs p n = iterateN n (step rs) init_map
  where
    init_map = M.fromListWith (+) $ zip (zip p $ tail p) $ repeat 1

polymerization :: Part -> Rules -> Polymer -> Integer
polymerization part rs p = (maximum counts - minimum counts) `div` 2
  where
    counts =
      map snd $
        M.toList $
          M.fromListWith (+) $
            (++ [(head p, 1), (last p, 1)]) $
              concatMap (\((l, r), n) -> [(l, n), (r, n)]) $
                M.toList $ synthesize rs p (if part == Part1 then 10 else 40)
