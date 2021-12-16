module Util
  ( parse,
    stdinParse,
    ws,
    tokenize,
    int,
    char,
    char',
    choice,
    string',
    sepBy,
    sepBy1,
    many1,
    (+++),
    (<++),
    ws1,
    digit,
    satisfy,
    satisfy',
    Parser,
    Part (..),
    Map,
    Array,
    (!?),
    (!),
    (//),
    iterateN,
    second,
    first,
    bimap,
    gridFromList,
    neighbors,
    neighborsDiag,
    putPart1,
    putPart2,
    fileParse,
    count,
    Bit,
    Bin,
    bin2int,
    look,
    gather,
    transformInput,
    module Control.Applicative,
    module Control.Monad,
    module Data.Maybe,
    module Data.Either,
  )
where

import Control.Applicative
import Control.Monad
import Data.Array (Array, bounds, listArray, (!), (//))
import Data.Bifunctor (bimap, first, second)
import Data.Char (isDigit, isSpace)
import Data.Either
import Data.Ix (Ix, inRange)
import Data.Map (Map)
import Data.Maybe
import System.IO
import Text.ParserCombinators.ReadP

type Parser a = ReadP a

transformInput :: (String -> String) -> Parser a -> Parser a
transformInput f = readS_to_P . (. f) . readP_to_S

parse :: Parser a -> String -> a
parse p s =
  case readP_to_S (ws >> p <* eof) s of
    [(a, mempty)] -> a
    (_ : _) -> error "Ambiguous parse."
    _ -> error "Failed parse."

stdinParse :: Parser a -> IO a
stdinParse p = parse p <$> getContents

fileParse :: FilePath -> Parser a -> IO a
fileParse fp p = parse p <$> readFile fp

ws :: Parser ()
ws = skipSpaces

ws1 :: Parser ()
ws1 = skipMany1 $ satisfy isSpace

tokenize :: Parser a -> Parser a
tokenize p = p <* ws

digit :: Parser Int
digit = (read . pure) <$> satisfy isDigit

int :: Parser Int
int = tokenize $ read <$> many1 (satisfy isDigit)

char' :: Char -> Parser Char
char' = tokenize . char

string' :: String -> Parser String
string' = tokenize . string

satisfy' :: (Char -> Bool) -> Parser Char
satisfy' = tokenize . satisfy

data Part = Part1 | Part2 deriving (Eq)

putPart1 :: Show a => a -> IO ()
putPart1 a = putStrLn $ "part1: " <> show a

putPart2 :: Show a => a -> IO ()
putPart2 a = putStrLn $ "part2: " <> show a

fromFile :: FilePath -> (String -> IO a) -> IO a
fromFile fp = (readFile fp >>=)

(!?) :: Ix i => Array i e -> i -> Maybe e
a !? i
  | inRange (bounds a) i = Just $ a ! i
  | otherwise = Nothing

iterateN :: Int -> (a -> a) -> a -> a
iterateN n f a
  | n <= 0 = a
  | otherwise = iterateN (n -1) f $ f a

gridFromList :: [[a]] -> Array (Int, Int) a
gridFromList ass@(as : _) =
  listArray ((0, 0), (length ass -1, length as - 1)) $ concat ass

neighbors :: Array (Int, Int) a -> (Int, Int) -> [(Int, Int)]
neighbors a (x, y) =
  filter
    (\(x, y) -> x >= x_min && x <= x_max && y >= y_min && y <= y_max)
    ([(x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y)])
  where
    ((x_min, y_min), (x_max, y_max)) = bounds a

neighborsDiag :: Array (Int, Int) a -> (Int, Int) -> [(Int, Int)]
neighborsDiag a (x, y) =
  filter
    (\(x, y) -> x >= x_min && x <= x_max && y >= y_min && y <= y_max)
    ( [(x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y)]
        ++ [(x - 1, y - 1), (x - 1, y + 1), (x + 1, y - 1), (x + 1, y + 1)]
    )
  where
    ((x_min, y_min), (x_max, y_max)) = bounds a

type Bit = Char

type Bin = [Bit]

bin2int :: Bin -> Int
bin2int = sum . zipWith (*) [2 ^ p | p <- [0 ..]] . reverse . map (read . pure)
