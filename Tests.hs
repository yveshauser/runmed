import RunningMedian
import Test.QuickCheck
import Math.Statistics

runmed_naive :: Int -> [Double] -> [Double]
runmed_naive k l 
  | length l < 2*k+1 = l
  | otherwise = begin_rule k l ++ moving_median k l ++ end_rule k l

moving_median _ [] = []
moving_median k l@(x:xs) 
  | length l < 2*k+1 = []
  | otherwise = let window = take (2*k+1) l in 
                median window : moving_median k xs

k = 3
prop_median_model xs = runmed k xs == runmed_naive k xs
qc1 = quickCheck (prop_median_model :: [Double] -> Bool)

