module Day17.Shot where

import Util

type Pos = (Int, Int)

type Step = Int

data Target = Target
  { xMin :: Int,
    xMax :: Int,
    yMin :: Int,
    yMax :: Int
  }
  deriving (Show, Eq)

data Probe = Probe
  { pos :: Pos,
    vX :: Int,
    vY :: Int
  }
  deriving (Show, Eq, Ord)

inputP :: Parser Target
inputP = do
  string' "target area:"
  string' "x="
  x_min <- int
  string' ".."
  x_max <- int
  string' ","
  string' "y="
  y_min <- int
  string' ".."
  y_max <- int
  return $ Target x_min x_max y_min y_max

steps :: Step -> Probe -> Probe
steps n p = Probe (x, y) v_x v_y
  where
    v_x = max 0 (vX p - n)
    v_y = vY p - n
    x = (fst $ pos p) + euler (vX p) - euler (max 0 (vX p - n))
    y = (snd $ pos p) + euler (vY p) - euler (min 0 (vY p - n))

inTarget :: Target -> Probe -> Bool
inTarget t (Probe (x, y) _ _) =
  xMin t <= x && x <= xMax t && yMin t <= y && y <= yMax t

hits :: Target -> [Probe]
hits t = do
  v_x <- [min_vx .. max_vx]
  v_y <- [- max_vy .. max_vy]
  let p = Probe (0, 0) v_x v_y
  guard (any (inTarget t . flip steps p) [0 .. max_n v_y])
  return p
  where
    min_vx = floor $ (sqrt (8 * (fromIntegral $ xMin t) + 1) + 1) / 2
    max_vx = xMax t
    max_vy = max (abs $ yMin t) (abs $ yMax t)
    max_n v_y = floor ((sqrt ((2 * fromIntegral v_y + 1) ^ 2 - 8 * fromIntegral (yMin t)) + 2 * fromIntegral v_y + 1) / 2) + 1

maxY :: Probe -> Int
maxY = euler . vY

shot :: Part -> Target -> Int
shot Part1 = maximum . map maxY . hits
shot Part2 = length . hits
