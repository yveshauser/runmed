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
runmed' l = runST $ init k l >>= \h -> 
            liftM2 (:) (take_median h) (mapM (\(x,o) -> step h x o) l')
	    where l' = zip xs os
	          xs = drop window_size l
		  os = map (flip mod $ window_size) [0..]

begin_rule :: [Double] -> [Double]
begin_rule l = take k l

end_rule :: [Double] -> [Double]
end_rule l = let n = (length l) - k in
             drop n l 

step :: IndexedHeap s -> Double -> Int -> ST s Double
step h x o = do i <- read_idx_into_heap h (o+1)
                x_i <- read_elem h i
                med <- read_elem h median
                write_elem h i x 
                rebuild_heap i x_i x med h
                take_median h

rebuild_heap :: Int -> Double -> Double -> Double -> IndexedHeap s -> ST s ()
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
	max_out_max_in j h = move_up_max j h >>= max_heapify h
  	min_out_min_in j h = move_up_min j h >>= min_heapify h
	max_out_min_in j h = push_to_max_root j h >> 
	                     swap maxheap_root median h >> 
			     do min_root <- read_elem h minheap_root 
	                        if min_root < x then swap median minheap_root h >> min_heapify h minheap_root
				else return ()
	min_out_max_in j h = push_to_min_root j h >> 
	                     swap minheap_root median h >> 
			     do max_root <- read_elem h maxheap_root
	                        if max_root > x then swap median maxheap_root h >> max_heapify h maxheap_root
				else return ()
	med_out_min_in j h = do min_root <- read_elem h minheap_root 
	                        if min_root < x  then swap j median h >> swap median minheap_root h >> min_heapify h minheap_root 
				else return ()
	med_out_max_in j h = do max_root <- read_elem h maxheap_root 
	                        if max_root > x then swap j median h >> swap median maxheap_root h >> max_heapify h maxheap_root 
				else return ()
	med_out_med_in _ _ = return ()
