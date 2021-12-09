module Day8.Display where

import Data.Bifunctor (second)
import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Util

data Segment = TL | T | TR | M | BL | BO | BR deriving (Eq, Ord, Show)

data Signal = A | B | C | D | E | F | G deriving (Eq, Ord, Show)

type Digit = Set Signal

data Display = Display
  { digits :: Set Digit,
    output :: [Digit]
  }
  deriving (Show)

segments :: [(Int, Set Segment)]
segments =
  [ (0, S.fromList [TL, T, TR, BL, BO, BR]),
    (1, S.fromList [TR, BR]),
    (2, S.fromList [T, TR, M, BL, BO]),
    (3, S.fromList [T, TR, M, BR, BO]),
    (4, S.fromList [TL, TR, M, BR]),
    (5, S.fromList [T, TL, M, BR, BO]),
    (6, S.fromList [T, TL, M, BL, BR, BO]),
    (7, S.fromList [T, TR, BR]),
    (8, S.fromList [TL, T, TR, M, BL, BO, BR]),
    (9, S.fromList [TL, T, TR, M, BO, BR])
  ]

segmentsM :: Map Int (Set Segment)
segmentsM = M.fromList segments

segmentsMR :: Map (Set Segment) Int
segmentsMR = M.fromList $ map (\(x, y) -> (y, x)) segments

type Constraint = (Signal, Set Segment)

type Constraints = Map Signal (Set Segment)

constrain :: Digit -> Set Constraint
constrain d = S.map constrainSignal d
  where
    constrainSignal :: Signal -> Constraint
    constrainSignal s =
      (s, S.unions $ S.fromList $ M.keys $ M.filterWithKey (\ss _ -> S.size ss == S.size d) segmentsMR)

constraints :: Set Digit -> Constraints
constraints = M.fromListWith S.intersection . S.toList . S.unions . S.map constrain

mappings :: Constraints -> [Map Signal Segment]
mappings = mappings' . M.toList
  where
    mappings' [] = [M.empty]
    mappings' ((s, segs) : cs) = do
      seg <- S.toList segs
      rest <- mappings' $ map (second (S.delete seg)) cs
      return $ M.insert s seg rest

solve :: Set Digit -> Map Signal Segment
solve ds = solve' (mappings $ constraints ds) ds
  where
    solve' (m : ms) ds =
      case mapM (segmentsMR M.!?) (S.toList $ S.map (S.map (m M.!)) ds) of
        Nothing -> solve' ms ds
        Just _ -> m

toInt :: Map Signal Segment -> [Digit] -> Int
toInt m = foldl (\x d -> digitToInt m d + x * 10) 0
  where
    digitToInt m = (segmentsMR M.!) . S.map (m M.!)

inputP :: Parser [Display]
inputP = many $ tokenize displayP
  where
    displayP = do
      ds <- tokenize $ S.fromList <$> sepBy1 digitP (char ' ')
      char' '|'
      out <- sepBy1 digitP (char ' ')
      return $ Display ds out
    digitP = S.fromList <$> many1 segmentP
    segmentP =
      choice
        [ char 'a' >> return A,
          char 'b' >> return B,
          char 'c' >> return C,
          char 'd' >> return D,
          char 'e' >> return E,
          char 'f' >> return F,
          char 'g' >> return G
        ]

display :: Part -> [Display] -> Int
display Part1 = sum . map unique
  where
    unique = length . filter (\d -> S.size d `elem` map (S.size . (segmentsM M.!)) [1, 4, 7, 8]) . output
display Part2 = sum . map output_sum
  where
    output_sum disp = toInt (solve $ digits disp) $ output disp
