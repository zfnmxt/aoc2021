module Diagnostic where

import Data.List (transpose)

type Bin = String

type Bit = Char

mostCommon :: Bin -> Bit
mostCommon b = if 2 * numOne >= length b then '1' else '0'
  where
    numOne = sum (map (read . pure) b)

neg :: Bit -> Bit
neg '0' = '1'
neg '1' = '0'

leastCommon :: Bin -> Bit
leastCommon = neg . mostCommon

rate :: (Bin -> Bit) -> [Bin] -> Bin
rate f = map f . transpose

bin2int :: Bin -> Int
bin2int = sum . zipWith (*) [2 ^ p | p <- [0 ..]] . reverse . map (read . pure)

find :: (Bin -> Bit) -> [Bin] -> Bin
find criteria bs = head $ foldl f bs [0 .. length bs - 1]
  where
    f [c] _ = [c]
    f cs n = filter (\c -> (c !! n) == criteria (transpose cs !! n)) cs

main :: IO ()
main = do
  input <- lines <$> getContents
  let gamma = rate mostCommon input
  let epsilon = rate leastCommon input
  putStrLn $ "part1: " <> show (bin2int gamma * bin2int epsilon)
  let oxygen = find mostCommon input
  let co2 = find leastCommon input
  putStrLn $ "part2: " <> show (bin2int co2 * bin2int oxygen)
