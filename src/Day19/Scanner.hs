module Day19.Scanner where

import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Matrix as M
import qualified Data.Set as S
import qualified Data.Vector as V
import Util

type Matrix = M.Matrix Int

type Vector = V.Vector Int

type Pos = Vector

data Neighbor = N
  { mat :: Matrix,
    loc :: Vector,
    rel_to :: Int
  }
  deriving (Show, Eq)

data Scanner = S
  { points :: Set Vector,
    neighs :: [Neighbor]
  }
  deriving (Show, Eq)

inputP :: Parser (Map Int Scanner)
inputP = Map.adjust (\s -> s {neighs = [N (M.identity 3) (V.fromList [0, 0, 0]) 0]}) 0 . Map.fromList <$> endBy scannerP ws
  where
    scannerP = do
      string' "--- scanner"
      id <- int
      string' "---"
      ps <- many pointP
      return (id, S (S.fromList ps) mempty)
    pointP = do
      x <- int
      char ','
      y <- int
      char ','
      z <- int
      return $ V.fromList [x, y, z]

rotMat :: [Matrix]
rotMat = do
  i <- [0 .. 2]
  i_neg <- [1, -1]
  j <- [0 .. 2]
  j_neg <- [1, -1]
  k <- [0 .. 2]
  k_neg <- [1, -1]
  guard (i /= j && j /= k && i /= k)
  return $ M.fromLists [mkrow i i_neg, mkrow j j_neg, mkrow k k_neg]
  where
    mkrow x n = replicate x 0 ++ [n] ++ replicate (2 - x) 0

apply :: Matrix -> Vector -> Vector
apply m = M.getMatrixAsVector . M.multStd2 m . M.colVector

vOp :: (Int -> Int -> Int) -> Vector -> Vector -> Vector
vOp op v1 v2 = V.fromList $ zipWith op (V.toList v1) (V.toList v2)

normalizeTo :: Int -> Int -> Map Int Scanner -> Map Int Scanner
normalizeTo from to ss
  | to >= Map.size ss = ss
  | from == to = normalizeTo from (to + 1) ss
  | null normalize' = normalizeTo from (to + 1) ss
  | otherwise = normalizeTo from (to + 1) $ Map.insert from (scanner {neighs = head normalize' : neighs scanner}) ss
  where
    normalize' = do
      (m, cs) <- scanner_cands
      c <- cs
      let bs' = S.map (vOp (+) c) (S.map (apply m) as)
          ol = bs' `S.intersection` bs
      guard (S.size ol >= 12)
      return $ N m c to
    scanner_cands = do
      a <- S.toList as
      m <- rotMat
      return (m, map (\b -> vOp (-) b (apply m a)) $ S.toList bs)
    to_scanner = ss Map.! to
    scanner = ss Map.! from
    bs = points to_scanner
    as = points scanner

normalize :: Map Int Scanner -> Map Int Scanner
normalize ss = foldl (\acc id -> normalizeTo id 0 acc) ss [1 .. Map.size ss - 1]

transform :: Map Int Scanner -> Int -> Map Int Scanner
transform ss id =
  Map.insert id (s {neighs = neighs'}) ss
  where
    s = ss Map.! id
    neighs' = map (\(m', c') -> N m' c' 0) $ transform' ss id 0 mempty
    transform' ss from to seen = do
      guard (from `notElem` seen)
      N m c to' <- neighs s
      if to' == to
        then return (m, c)
        else do
          (m', c') <- transform' ss to' to (from : seen)
          return (m' `M.multStd2` m, vOp (+) (apply m' c) c')
      where
        s = ss Map.! from

transformAll :: Map Int Scanner -> Map Int Scanner
transformAll ss = foldl transform ss [1 .. Map.size ss - 1]

allPoints :: Map Int Scanner -> Set Vector
allPoints =
  S.unions . Map.elems
    . Map.map
      (\s -> S.map (vOp (+) (loc $ head $ neighs s) . apply (mat $ head $ neighs s)) (points s))

manhattan :: Vector -> Vector -> Int
manhattan v1 v2 = sum (abs <$> vOp (-) v1 v2)

scanner :: Part -> Map Int Scanner -> Int
scanner Part1 = length . allPoints . transformAll . normalize
scanner Part2 = maximum . (\ss -> [manhattan v1 v2 | v1 <- ss, v2 <- ss]) . allScanners . transformAll . normalize
  where
    allScanners = Map.elems . Map.map (loc . head . filter ((== 0) . rel_to) . neighs)
