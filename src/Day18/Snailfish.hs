module Day18.Snailfish where

import Data.List (delete)
import Util

data Snailfish = Val Int | Pair Snailfish Snailfish deriving (Eq, Ord)

instance Show Snailfish where
  show (Val x) = show x
  show (Pair l r) = "[" ++ show l ++ "," ++ show r ++ "]"

inputP :: Parser [Snailfish]
inputP = endBy snailfishP ws
  where
    snailfishP = do
      char '['
      x <- (Val <$> int) <|> snailfishP
      char ','
      y <- (Val <$> int) <|> snailfishP
      char ']'
      return $ Pair x y

explode :: Snailfish -> Snailfish
explode = (\(_, s, _) -> s) . explode' 0
  where
    explode' 4 (Pair (Val l) (Val r)) = (l, Val 0, r)
    explode' n (Val x) = (0, Val x, 0)
    explode' n (Pair l r) =
      let (llv, l', lrv) = explode' (n + 1) l
          (rlv, r', rrv) = explode' (n + 1) $ addLeftmost lrv r
       in (llv, addRightmost rlv l' `Pair` r', rrv)

    addLeftmost n (Val x) = Val $ n + x
    addLeftmost n (Pair l r) = addLeftmost n l `Pair` r

    addRightmost n (Val x) = Val $ n + x
    addRightmost n (Pair l r) = l `Pair` addRightmost n r

splitOne :: Snailfish -> Snailfish
splitOne (Val x)
  | x >= 10 = Pair (Val $ floor $ fromIntegral x / 2) (Val $ ceiling $ fromIntegral x / 2)
  | otherwise = Val x
splitOne (Pair l r)
  | splitOne l == l = l `Pair` splitOne r
  | otherwise = splitOne l `Pair` r

reduce :: Snailfish -> Snailfish
reduce s
  | s' /= s = reduce s'
  | otherwise = s
  where
    s' = splitOne $ explode s

add :: Snailfish -> Snailfish -> Snailfish
add s1 s2 = reduce $ Pair s1 s2

magnitude :: Snailfish -> Int
magnitude (Val x) = x
magnitude (Pair l r) = 3 * magnitude l + 2 * magnitude r

snailfish :: Part -> [Snailfish] -> Int
snailfish Part1 ss = magnitude $ foldl1 add ss
snailfish Part2 ss = maximum [magnitude $ s1 `add` s2 | s1 <- ss, s2 <- s1 `delete` ss]
