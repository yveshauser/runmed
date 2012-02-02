import Median
import IndexedHeap hiding ( median )

import Test.QuickCheck
import Math.Statistics

import Data.Array.ST
import Data.Array.Unboxed
import Data.List

hsort :: [Double] -> [Double]
hsort [] = []
hsort l = elems $ runSTUArray $ do s <- build_from_list l
                                   heapsort (length l) s 
	                           return $ heap s

prop_sort_model xs = sort xs == hsort xs
qc1 = quickCheck (prop_sort_model :: [Double] -> Bool)

runmed_naive :: [Double] -> [Double]
runmed_naive l 
  | length l < 2*k+1 = l
  | otherwise = begin_rule l ++ moving_median l ++ end_rule l

moving_median [] = []
moving_median l@(x:xs) 
  | length l < 2*k+1 = []
  | otherwise = let window = take (2*k+1) l in 
                median window : moving_median xs

prop_median_model xs = runmed xs == runmed_naive xs
qc2 = quickCheck (prop_median_model :: [Double] -> Bool)


