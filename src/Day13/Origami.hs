module Day13.Origami where

import qualified Data.List as L
import Util

type Point = (Int, Int)

data Fold = X Int | Y Int

type Origami = ([Point], [Fold])

inputP :: Parser Origami
inputP = (,) <$> paperP <*> foldP
  where
    paperP = many1 $ do
      x <- int
      char ','
      y <- int
      return (x, y)
    foldP = many1 $ do
      string' "fold along"
      axis <- char 'x' <|> char 'y'
      char '='
      xory <- int
      case axis of
        'x' -> return $ X xory
        'y' -> return $ Y xory
        _ -> error ""

foldPaper :: [Point] -> Fold -> [Point]
foldPaper ps f = [transform f p | p <- ps]
  where
    transform (X fold_x) (x, y)
      | x > fold_x = (2 * fold_x - x, y)
    transform (Y fold_y) (x, y)
      | y > fold_y = (x, 2 * fold_y - y)
    transform _ (x, y) = (x, y)

foldOrigami :: Origami -> [Point]
foldOrigami = uncurry $ foldl foldPaper

printPaper :: [Point] -> String
printPaper ps =
  unlines
    [ map (\x -> if x `elem` map fst row then '█' else ' ') [0 .. maximum $ map fst ps]
      | row <- L.groupBy (\(_, y1) (_, y2) -> y1 == y2) $ L.sortOn snd ps
    ]

origami :: Part -> Origami -> Either Int String
origami Part1 (p, f : _) = Left $ length $ foldPaper p f
origami Part2 o = Right $ printPaper $ foldOrigami o
