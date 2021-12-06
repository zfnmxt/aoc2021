module Util
  ( parse,
    stdinParse,
    ws,
    tokenize,
    int,
    char',
    string',
    sepBy,
    Parser,
    Part (..),
    module Control.Applicative,
    module Control.Monad,
  )
where

import Control.Applicative
import Control.Monad
import Data.Char (isDigit)
import System.IO
import Text.ParserCombinators.ReadP

type Parser a = ReadP a

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

tokenize :: Parser a -> Parser a
tokenize p = p <* ws

int :: Parser Int
int = tokenize $ read <$> many1 (satisfy isDigit)

char' :: Char -> Parser Char
char' = tokenize . char

string' :: String -> Parser String
string' = tokenize . string

data Part = Part1 | Part2

putPart1 :: Show a => a -> IO ()
putPart1 a = putStrLn $ "part1: " <> show a

putPart2 :: Show a => a -> IO ()
putPart2 a = putStrLn $ "part2: " <> show a

fromFile :: FilePath -> (String -> IO a) -> IO a
fromFile fp = (readFile fp >>=)
