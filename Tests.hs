{-# LANGUAGE FlexibleInstances #-}

import RunningMedian
import Test.QuickCheck
import Math.Statistics

import qualified Data.Vector.Unboxed as V

type Vector = V.Vector Double

instance Arbitrary (V.Vector Double) where
    arbitrary = fmap V.fromList arbitrary

runmed_naive :: Int -> Vector -> Vector
runmed_naive k l 
  | V.length l < 2*k+1 = l
  | otherwise = begin_rule k l `cat` moving_median k l `cat` end_rule k l
	where cat = (V.++)

moving_median :: Int -> Vector -> Vector
moving_median k v = V.fromList $ moving_median' k (V.toList v)

moving_median' _ [] = []
moving_median' k l@(x:xs) 
  | length l < 2*k+1 = []
  | otherwise = let window = take (2*k+1) l in 
                median window : moving_median' k xs

k = 3

prop_median_model xs = runmed k xs == runmed_naive k xs
qc1 = quickCheck (prop_median_model :: Vector -> Bool)
