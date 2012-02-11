import Median
import IndexedHeap hiding ( median )

import Test.QuickCheck
--import Math.Statistics

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.List

build_from_list :: [Double] -> ST s (IndexedHeap s)
build_from_list l = liftM3 IndexedHeap heap idx_into_heap idx_into_window 
	   where
             heap = newListArray (1, up_idx) l
	     idx_into_heap = newListArray (1, up_idx) [1 .. up_idx]
	     idx_into_window = newListArray (1, up_idx) [1 .. up_idx]
	     up_idx = length l

hsort :: [Double] -> [Double]
hsort [] = []
hsort l = elems $ runSTUArray $ do s <- build_from_list l
				   let ?heap = s
                                   heapsort (length l) 
	                           return $ heap ?heap

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
                median' window : moving_median xs

prop_median_model xs = runmed xs == runmed_naive xs
qc2 = quickCheck (prop_median_model :: [Double] -> Bool)

-- copied from hstats (cabal install failed...)
mean :: Floating a => [a] -> a
mean x = fst $ foldl' (\(!m, !n) x -> (m+(x-m)/(n+1),n+1)) (0,0) x

median' :: (Floating a, Ord a) => [a] -> a
median' x | odd n  = head  $ drop (n `div` 2) x'
         | even n = mean $ take 2 $ drop i x'
                  where i = (length x' `div` 2) - 1
                        x' = sort x
                        n  = length x


--runmed_naive :: Num a => [a] -> [a]

