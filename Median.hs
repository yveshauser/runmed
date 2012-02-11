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

runmed :: [Double] -> [Double]
runmed l  
  | length l < 2*k+1 = l
  | otherwise = begin_rule l ++ runmed' l ++ end_rule l

runmed' :: [Double] -> [Double]
runmed' l = runST $ do 
            h <- init k l 
	    let ?heap = h 
            liftM2 (:) take_median (mapM (\(x,o) -> step x o) l')
	    where l' = zip xs os
	          xs = drop window_size l
		  os = map (flip mod $ window_size) [0..]

begin_rule :: [Double] -> [Double]
begin_rule l = take k l

end_rule :: [Double] -> [Double]
end_rule l = let n = (length l) - k in
             drop n l 

step :: (?heap :: IndexedHeap s ) => Double -> Int -> ST s Double
step x o = do i <- read_idx_into_heap (o+1) 
              x_i <- read_elem i
              med <- read_elem median
              write_elem i x 
              rebuild_heap i x_i x med
              take_median 

rebuild_heap :: (?heap :: IndexedHeap s) => Int -> Double -> Double -> Double -> ST s ()
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
	max_out_min_in j = push_to_max_root j >> 
	                   swap maxheap_root median >> 
			   do min_root <- read_elem minheap_root 
	                      if min_root < x then swap median minheap_root >> min_heapify minheap_root
			      else return ()
	min_out_max_in j = push_to_min_root j >> 
	                   swap minheap_root median >> 
			   do max_root <- read_elem maxheap_root
	                      if max_root > x then swap median maxheap_root >> max_heapify maxheap_root
			      else return ()
	med_out_min_in j = do min_root <- read_elem minheap_root 
	                      if min_root < x  then swap j median >> swap median minheap_root >> min_heapify minheap_root 
			      else return ()
	med_out_max_in j = do max_root <- read_elem maxheap_root 
	                      if max_root > x then swap j median >> swap median maxheap_root >> max_heapify maxheap_root 
			      else return ()
	med_out_med_in _ = return ()
