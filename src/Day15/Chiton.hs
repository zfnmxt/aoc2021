module Day15.Chiton where

import qualified Data.Array as A
import Data.PSQueue (Binding (..), PSQ)
import qualified Data.PSQueue as Q
import Data.Set (Set)
import qualified Data.Set as S
import Util

type Pos = (Int, Int)

data D a = D a | Inf deriving (Show, Eq, Ord)

data Node = Node
  { dist :: D Int,
    risk :: Int
  }
  deriving (Eq, Show)

instance Ord Node where
  n1 <= n2 = dist n1 <= dist n2

type Cavern = Array Pos Node

fromD :: D a -> a
fromD (D a) = a
fromD _ = error ""

inputP :: Parser Cavern
inputP = gridFromList <$> sepBy (many1 nodeP) (char '\n')
  where
    nodeP = Node Inf <$> digit

add_dist :: Num a => D a -> a -> D a
add_dist (D s) t = D $ s + t
add_dist _ _ = Inf

dijkstra :: Pos -> Cavern -> Cavern
dijkstra start c = dijkstra' init_c init_queue init_set
  where
    init_c = c // [(start, (c ! start) {dist = D 0})]
    init_queue = Q.fromList $ map (uncurry (:->)) $ A.assocs init_c
    init_set = S.fromList $ A.range $ A.bounds init_c
    dijkstra' c q ps
      | Q.null q = c
      | otherwise = dijkstra' (c // upd) q'' ps'
      where
        Just (s :-> n_s, q') = Q.minView q
        ps' = s `S.delete` ps
        ts = filter (`S.member` ps) $ neighbors c s
        upd = do
          t <- ts
          let n_s = c ! s
              n_t = c ! t
              new_length = dist n_s `add_dist` risk n_t
          guard $ new_length < dist n_t
          return (t, n_t {dist = new_length})
        q'' = foldl (\x (p, n) -> Q.insert p n x) q' upd

expand :: Cavern -> Cavern
expand c = A.array new_bounds $ concatMap (\(i, j) -> map (uncurry $ f (i, j)) c_list) indices
  where
    c_list = A.assocs c
    (start, (max_x, max_y)) = A.bounds c
    new_bounds = (start, ((max_x + 1) * 5 -1, (max_y + 1) * 5 - 1))
    indices = [(i, j) | i <- [0 .. 4], j <- [0 .. 4]]
    f (i, j) (x, y) n =
      let x' = x + i * (max_x + 1)
          y' = y + j * (max_y + 1)
          r = risk n + i + j
          r'
            | r > 9 = r - 9
            | otherwise = r
       in ((x', y'), n {risk = r'})

chiton :: Part -> Cavern -> Int
chiton part c = fromD $ dist $ dijkstra (0, 0) c' ! (snd $ A.bounds c')
  where
    c' = if part == Part1 then c else expand c
