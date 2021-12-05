module Day1.Sonar where

increases :: Ord a => [a] -> [Ordering]
increases as = zipWith compare (tail as) as

countGT :: [Ordering] -> Int
countGT = length . filter (== GT)

windows :: [a] -> [(a, a, a)]
windows as = zip3 as (tail as) (tail $ tail as)

windowIncreases :: (Num a, Ord a) => [a] -> [Ordering]
windowIncreases as = increases $ map (\(a1, a2, a3) -> a1 + a2 + a3) $ windows as

main :: IO ()
main = do
  input <- map read <$> words <$> getContents
  putStrLn $ "increases: " <> show (countGT $ increases $ input)
  putStrLn $ "window increases: " <> show (countGT $ windowIncreases $ input)
