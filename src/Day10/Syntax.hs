module Day10.Syntax where

import Data.List (sort)
import qualified Data.Map as M
import Util

open :: [Char]
open = "([{<"

close :: [Char]
close = ")]}>"

m :: Map Char Char
m = M.fromList $ zip open close

inputP :: Parser [String]
inputP = tokenize $ sepBy1 (many1 (satisfy (`elem` (open ++ close)))) ws1

scope :: String -> Either Char String
scope = scope' mempty
  where
    scope' ss "" = return ss
    scope' ss (c : cs)
      | c `elem` open = scope' (c : ss) cs
      | not (null ss) && m M.! head ss == c = scope' (tail ss) cs
      | otherwise = Left c

corrupted :: String -> Bool
corrupted = isLeft . scope

complete :: [String] -> [String]
complete = (map . map) (m M.!) . rights . map scope

score :: Part -> Char -> Int
score Part1 = (M.fromList [(')', 3), (']', 57), ('}', 1197), ('>', 25137)] M.!)
score Part2 = (M.fromList [(')', 1), (']', 2), ('}', 3), ('>', 4)] M.!)

syntax :: Part -> [String] -> Int
syntax Part1 = sum . map (score Part1) . lefts . map scope
syntax Part2 = (\l -> l !! (length l `div` 2)) . sort . map scoreLine . complete . incomplete
  where
    incomplete = filter (not . corrupted)
    scoreLine = foldl (\t x -> t * 5 + score Part2 x) 0
