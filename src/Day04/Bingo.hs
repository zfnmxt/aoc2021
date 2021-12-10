module Day04.Bingo where

import Data.Either
import Data.List (groupBy, transpose)

type Board = [[Either Int Int]]

parse :: String -> ([Int], [Board])
parse input = (draws, boards)
  where
    (ds : bs) = lines input
    draws = map read $ words $ map (\c -> if c == ',' then ' ' else c) ds
    boards =
      map (tail . map (map (Right . read) . words)) $
        groupBy (\l r -> r /= mempty) bs

mark :: Int -> Board -> Board
mark n = map $ map $ \x -> do v <- x; if v == n then Left v else x

won :: Board -> Bool
won b = any (all isLeft) b || any (all isLeft) (transpose b)

win :: [Int] -> [Board] -> (Int, Board)
win (n : ns) bs =
  let bs' = map (mark n) bs
   in if any won bs'
        then (n, head $ filter won bs')
        else win ns bs'
win _ _ = error ""

lose :: [Int] -> [Board] -> (Int, Board)
lose ns [b] = win ns [b]
lose (n : ns) bs = lose ns $ filter (not . won) $ map (mark n) bs
lose _ _ = error ""

score :: Int -> Board -> Int
score last winner = last * unmarked_sum
  where
    unmarked_sum = sum $ rights $ concat winner

main :: IO ()
main = do
  (draws, boards) <- parse <$> getContents
  putStrLn $ "part1 :" <> show (uncurry score $ win draws boards)
  putStrLn $ "part2 :" <> show (uncurry score $ lose draws boards)
