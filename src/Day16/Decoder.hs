module Day16.Decoder where

import qualified Data.Map as M
import Util

data Packet a = Packet
  { version :: a,
    typeID :: a,
    payload :: Payload a
  }
  deriving (Eq, Show)

data Payload a = Val a | PS [Packet a] deriving (Eq, Show)

type Hex = Char

isBin :: Char -> Bool
isBin '0' = True
isBin '1' = True
isBin _ = False

hexMap :: Map Hex Bin
hexMap =
  M.fromList
    [ ('0', "0000"),
      ('1', "0001"),
      ('2', "0010"),
      ('3', "0011"),
      ('4', "0100"),
      ('5', "0101"),
      ('6', "0110"),
      ('7', "0111"),
      ('8', "1000"),
      ('9', "1001"),
      ('A', "1010"),
      ('B', "1011"),
      ('C', "1100"),
      ('D', "1101"),
      ('E', "1110"),
      ('F', "1111")
    ]

hexToBin :: [Hex] -> Bin
hexToBin = concatMap (hexMap M.!)

binToIntP :: Int -> Parser Int
binToIntP n = bin2int <$> count n (satisfy isBin)

parseNBits :: Int -> Parser a -> Parser [a]
parseNBits bits p = do
  (s, a) <- gather p
  let bits' = bits - length s
  if bits' > 0
    then fmap (a :) (parseNBits bits' p)
    else return [a]

opP :: Parser (Payload Int)
opP = do
  ltID <- binToIntP 1
  PS <$> case ltID of
    0 -> do
      bits <- binToIntP 15
      parseNBits bits packetP
    1 -> do
      num_packets <- binToIntP 11
      count num_packets packetP

litP :: Parser (Payload Int)
litP = Val . bin2int <$> litP'
  where
    litP' = do
      (b : bs) <- parseNBits 5 (satisfy isBin)
      case b of
        '0' -> return bs
        '1' -> (bs ++) <$> litP'

packetP :: Parser (Packet Int)
packetP = do
  v <- binToIntP 3
  t <- binToIntP 3
  p <- payloadP t
  return $ Packet v t p
  where
    payloadP 4 = litP
    payloadP _ = opP

inputP :: String -> Packet Int
inputP = parse (packetP <* many (char '0')) . hexToBin

eval :: Packet Int -> Int
eval (Packet _v t ps) = f t $ eval_payload ps
  where
    eval_payload (Val x) = [x]
    eval_payload (PS ps) = map eval ps
    ps' = eval_payload ps
    f 0 = sum
    f 1 = product
    f 2 = minimum
    f 3 = maximum
    f 4 = head
    f 5 = \[x, y] -> if x > y then 1 else 0
    f 6 = \[x, y] -> if x < y then 1 else 0
    f 7 = \[x, y] -> if x == y then 1 else 0

decoder :: Part -> Packet Int -> Int
decoder Part1 = sum . map version . all_packets
  where
    all_packets p@(Packet _ _ Val {}) = [p]
    all_packets p@(Packet _ _ (PS ps)) = p : concatMap all_packets ps
decoder Part2 = eval
