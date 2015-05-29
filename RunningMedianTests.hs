{-# LANGUAGE FlexibleInstances #-}

import RunningMedian
import Test.QuickCheck
import Math.Statistics

import Data.List
import Data.Ord (comparing)

runmed_naive :: Int -> [Double] -> [Double]
runmed_naive k l
  | length l < 2*k+1 = l
  | otherwise = begin_rule k l ++ moving_median k l ++ end_rule k l
  where
    begin_rule = take
    end_rule k l = let n = length l - k in drop n l

moving_median _ [] = []
moving_median k l@(x:xs)
  | length l < 2*k+1 = []
  | otherwise = let window = take (2*k+1) l in
                median window : moving_median k xs

prop_length k xs = k >= 3 && odd k ==> length (runmed k xs) == length xs
qc0 = quickCheck prop_length

prop_median_model xs = let k = 3 in runmed k xs == runmed_naive k xs
qc1 = quickCheck (prop_median_model :: [Double] -> Bool)

prop_median_model_fixed k xs = k >= 3 && odd k ==> runmed k xs == runmed_naive k xs
qc2 = quickCheck prop_median_model_fixed
