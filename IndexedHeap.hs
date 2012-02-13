module IndexedHeap 
where

import Prelude hiding ( head )

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Bits
import Data.STRef

data IndexedHeap s = IndexedHeap {
 	    elems :: STUArray s Int Double
 	  , idx_into_elems :: STUArray s Int Int
 	  , idx_into_window :: STUArray s Int Int
 	} 

read_elem :: (?heap :: IndexedHeap s) => Int -> ST s Double
read_elem = readArray (elems ?heap)

write_elem :: (?heap :: IndexedHeap s) => Int -> Double -> ST s ()
write_elem = writeArray (elems ?heap)

read_idx_into_elems :: (?heap :: IndexedHeap s) => Int -> ST s Int
read_idx_into_elems = readArray (idx_into_elems ?heap)

write_idx_into_elems :: (?heap :: IndexedHeap s) => Int -> Int -> ST s ()
write_idx_into_elems = writeArray (idx_into_elems ?heap)

read_idx_into_window :: (?heap :: IndexedHeap s) => Int -> ST s Int
read_idx_into_window = readArray (idx_into_window ?heap)

write_idx_into_window :: (?heap :: IndexedHeap s) => Int -> Int -> ST s ()
write_idx_into_window = writeArray (idx_into_window ?heap)

swap :: (?heap :: IndexedHeap s) => Int -> Int -> ST s ()
swap i j = do -- read values
              heap_elem_i <- read_elem i
              heap_elem_j <- read_elem j
	      win_elem_i <- read_idx_into_window i
	      win_elem_j <- read_idx_into_window j
	      pos_elem_k <- read_idx_into_elems win_elem_i
	      pos_elem_l <- read_idx_into_elems win_elem_j
	      -- update heap
              write_elem j heap_elem_i 
              write_elem i heap_elem_j 
	      -- update position index
	      write_idx_into_elems win_elem_i pos_elem_l
	      write_idx_into_elems win_elem_j pos_elem_k
	      -- update window index
	      write_idx_into_window j win_elem_i
	      write_idx_into_window i win_elem_j

{-# INLINE left #-}
left :: Int -> Int
left i = shiftL i 1

{-# INLINE right #-}
right :: Int -> Int
right i = succ $ shiftL i 1

{-# INLINE parent #-}
parent :: Int -> Int
parent i = shiftR i 1

build_max_heap :: (?heap :: IndexedHeap s, ?ctx :: Ctx) => Int -> ST s ()
build_max_heap s = mapM_ (heapify Max s) $ reverse [1 .. up_idx]
         where up_idx = div s 2

heapsort :: (?heap :: IndexedHeap s, ?ctx :: Ctx) => Int -> ST s ()
heapsort 1 = return ()
heapsort n = build_max_heap n >> swap 1 n >> heapsort (n-1)

type Rel = Double -> Double -> Bool
data Prio = Min | Max

heapify :: (?heap :: IndexedHeap s, ?ctx :: Ctx) => Prio -> Int -> Int -> ST s ()
heapify p s i = do l <- heapify_l p s i 
                   m <- heapify_r p s i l 
	  	   if not (m == i)
		   then swap i m >> heapify p s m
		   else return ()

heapify_l :: (?heap :: IndexedHeap s, ?ctx :: Ctx) => Prio -> Int -> Int -> ST s Int
heapify_l Min s i = let l = left (i-(o-1)) + (o-1) in
		    if l > (s + (o-1)) then return i
		    else idx_of_p (<) l i 
		where o = idx_minheap_root

heapify_l Max s i = let l = left i in
	            if l > s then return i
		    else idx_of_p (>) l i 

heapify_r :: (?heap :: IndexedHeap s, ?ctx :: Ctx) => Prio -> Int -> Int -> Int -> ST s Int
heapify_r Min s i m = let r = right (i-(o-1)) + (o-1) in
		      if r > (s + (o-1)) then return m
		      else idx_of_p (<) r m
		where o = idx_minheap_root

heapify_r Max s i largest = let r = right i in
		            if r > s then return largest
			    else idx_of_p (>) r largest

idx_of_p :: (?heap :: IndexedHeap s) => Rel -> Int -> Int -> ST s Int
idx_of_p r i j = do elem_i <- read_elem i
		    elem_j <- read_elem j
		    if r elem_i elem_j then return i
		    else return j

push_to_idx :: (?heap :: IndexedHeap s) => Int -> Int -> ST s ()
push_to_idx r i 
	| i == r = return ()
	| otherwise = let j = parent (i-(r-1)) + (r-1) in
		      swap i j >> push_to_idx r j

move_up :: (?heap :: IndexedHeap s) => Rel -> Int -> Int -> ST s Int
move_up r o i = do let p = parent (i-(o-1)) + (o-1)
	           if (o > p) then return i
	           else do elem_i <- read_elem i
		           elem_p <- read_elem p
                           if r elem_p elem_i 
		           then swap i p >> move_up r o p
		           else return i

-- Also having a context as implicit parameter, in order to statically
-- get the window size and dependent parameters

data Ctx = C { heap_size' :: Int 
             , idx_median' :: Int
	     , idx_maxheap_root' :: Int
	     , idx_minheap_root' :: Int
       	     , window_size' :: Int
            }		

buildCtx :: Int -> Ctx
buildCtx k = C k (k+1) 1 (k+2) (2*k+1)

idx_median :: (?ctx :: Ctx) => Int
idx_median = idx_median' ?ctx

idx_maxheap_root :: (?ctx :: Ctx) => Int
idx_maxheap_root = idx_maxheap_root' ?ctx

idx_minheap_root :: (?ctx :: Ctx) => Int
idx_minheap_root = idx_minheap_root' ?ctx

heap_size :: (?ctx :: Ctx) => Int
heap_size = heap_size' ?ctx

window_size :: (?ctx :: Ctx) => Int
window_size = window_size' ?ctx

max_heapify :: (?heap :: IndexedHeap s, ?ctx :: Ctx) => Int -> ST s ()
max_heapify = heapify Max heap_size

min_heapify :: (?heap :: IndexedHeap s, ?ctx :: Ctx) => Int -> ST s ()
min_heapify = heapify Min heap_size

move_up_max :: (?heap :: IndexedHeap s, ?ctx :: Ctx) => Int -> ST s Int
move_up_max = move_up (<) idx_maxheap_root

move_up_min :: (?heap :: IndexedHeap s, ?ctx :: Ctx) => Int -> ST s Int
move_up_min = move_up (>) idx_minheap_root

take_median :: (?heap :: IndexedHeap s, ?ctx :: Ctx) => ST s Double
take_median = readArray (elems ?heap) idx_median

push_to_max_root :: (?heap :: IndexedHeap s, ?ctx :: Ctx) => Int -> ST s ()
push_to_max_root = push_to_idx idx_maxheap_root

push_to_min_root :: (?heap :: IndexedHeap s, ?ctx :: Ctx) => Int -> ST s ()
push_to_min_root = push_to_idx idx_minheap_root 

-- Construction of the data structure

init :: (?heap :: IndexedHeap s, ?ctx :: Ctx) => [Double] -> ST s ()
init l = heapsort window_size >> reverse idx_maxheap_root heap_size
	where reverse i j 
		| i < j = swap i j >> reverse (i+1) (j-1)
		| otherwise = return ()

build :: (?ctx :: Ctx) => [Double] -> ST s (IndexedHeap s)
build l = liftM3 IndexedHeap heap idx_into_heap idx_into_window
	  where
             heap = newListArray (1, up_idx) l
	     idx_into_heap = newListArray (1, up_idx) [1 .. up_idx]
	     idx_into_window = newListArray (1, up_idx) [1 .. up_idx]
	     up_idx = window_size

