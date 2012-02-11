module Median (
	  runmed
        , begin_rule
        , end_rule
	)
where

import Prelude hiding ( init )
import Control.Monad
import Control.Monad.ST

import IndexedHeap 

-- Implementation of a running median smoother according to the
-- algorithm described in Haerdle und Steiger (1995).
runmed :: Int -> [Double] -> [Double]
runmed k l  
  | length l < 2*k+1 = l
  | otherwise = begin_rule k l ++ runmed' k l ++ end_rule k l

runmed' :: Int -> [Double] -> [Double]
runmed' k l = let ?ctx = buildCtx k in runST $ 
	      do h <- init k l 
	         let ?heap = h 
                 liftM2 (:) take_median (mapM (\(x,o) -> step x o) l')
	         where l' = zip xs os
	               xs = drop ws l
		       os = map (flip mod $ ws) [0..]
                       ws = 2*k+1

begin_rule :: Int -> [Double] -> [Double]
begin_rule k l = take k l

end_rule :: Int -> [Double] -> [Double]
end_rule k l = let n = (length l) - k in drop n l 

step :: (?heap :: IndexedHeap s, ?ctx :: Ctx) => Double -> Int -> ST s Double
step x o = do i <- read_idx_into_heap (o+1) 
              x_i <- read_elem i
              med <- read_elem idx_median
              write_elem i x 
              rebuild_heap i x_i x med
              take_median 

rebuild_heap :: (?heap :: IndexedHeap s, ?ctx :: Ctx) => Int -> Double -> Double -> Double -> ST s ()
rebuild_heap i x_i x med 
        -- min out 
        | x_i > med && x >= med = min_out_min_in i
        | x_i > med && x <  med = min_out_max_in i
        -- max out
        | x_i < med && x <= med = max_out_max_in i
        | x_i < med && x >  med = max_out_min_in i
        -- median out
        | x_i == med && x >  med = med_out_min_in i
        | x_i == med && x <  med = med_out_max_in i
        | x_i == med && x == med = med_out_med_in i
  where 
	max_out_max_in j = move_up_max j >>= max_heapify
  	min_out_min_in j = move_up_min j >>= min_heapify
	max_out_min_in j = push_to_max_root j >> swap idx_maxheap_root idx_median >> 
			   do min_root <- read_elem idx_minheap_root 
	                      if min_root < x then swap idx_median idx_minheap_root >> min_heapify idx_minheap_root
			      else return ()
	min_out_max_in j = push_to_min_root j >> swap idx_minheap_root idx_median >> 
			   do max_root <- read_elem idx_maxheap_root
	                      if max_root > x then swap idx_median idx_maxheap_root >> max_heapify idx_maxheap_root
			      else return ()
	med_out_min_in j = do min_root <- read_elem idx_minheap_root 
	                      if min_root < x  then swap j idx_median >> swap idx_median idx_minheap_root >> min_heapify idx_minheap_root 
			      else return ()
	med_out_max_in j = do max_root <- read_elem idx_maxheap_root 
	                      if max_root > x then swap j idx_median >> swap idx_median idx_maxheap_root >> max_heapify idx_maxheap_root 
			      else return ()
	med_out_med_in _ = return ()
