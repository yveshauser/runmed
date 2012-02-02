module IndexedHeap {-(
	  IndexedHeap(..)
	, build
	, swap
	, max_heapify
	, min_heapify
	, move_up
	, push_to_root
	, heapsort
	)-}
where

import Prelude hiding ( head )

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Bits
import Data.STRef

data IndexedHeap s = IndexedHeap {
 	    heap :: STUArray s Int Double
 	  , idx_into_heap :: STUArray s Int Int
 	  , idx_into_window :: STUArray s Int Int
 	} 

read_elem s = readArray (heap s)
write_elem s = writeArray (heap s)
read_idx_into_heap s = readArray (idx_into_heap s)
read_idx_into_window s = readArray (idx_into_window s)

swap :: Int -> Int -> IndexedHeap s -> ST s ()
swap i j s = do -- read values
                heap_elem_i <- readArray (heap s) i
                heap_elem_j <- readArray (heap s) j
		win_elem_i <- readArray (idx_into_window s) i
		win_elem_j <- readArray (idx_into_window s) j
		pos_elem_k <- readArray (idx_into_heap s) win_elem_i
		pos_elem_l <- readArray (idx_into_heap s) win_elem_j
		-- update heap
                writeArray (heap s) j heap_elem_i 
                writeArray (heap s) i heap_elem_j 
		-- update position index
		writeArray (idx_into_heap s) win_elem_i pos_elem_l
		writeArray (idx_into_heap s) win_elem_j pos_elem_k
		-- update window index
		writeArray (idx_into_window s) j win_elem_i
		writeArray (idx_into_window s) i win_elem_j

build_from_list :: [Double] -> ST s (IndexedHeap s)
build_from_list l = liftM3 IndexedHeap heap idx_into_heap idx_into_window 
	   where
             heap = newListArray (1, up_idx) l
	     idx_into_heap = newListArray (1, up_idx) [1 .. up_idx]
	     idx_into_window = newListArray (1, up_idx) [1 .. up_idx]
	     up_idx = length l

{-# INLINE left #-}
left :: Int -> Int
left i = shiftL i 1

{-# INLINE right #-}
right :: Int -> Int
right i = succ $ shiftL i 1

{-# INLINE parent #-}
parent :: Int -> Int
parent i = shiftR i 1

build_max_heap :: Int -> IndexedHeap s -> ST s ()
build_max_heap size s = let idx = floor (fromIntegral(size)/2) in 
                        mapM_ (\i -> heapify (>) maxheap_root size s i) $ reverse [1 .. idx]

heapsort :: Int -> IndexedHeap s -> ST s (IndexedHeap s)
heapsort 1 s = return s
heapsort n s = build_max_heap n s >> swap 1 n s >> heapsort (n-1) s

type Rel = Double -> Double -> Bool

max_heapify' :: Int -> IndexedHeap s -> ST s ()
max_heapify' s c = heapify (>) maxheap_root heap_size c s 

min_heapify' :: Int -> IndexedHeap s -> ST s ()
min_heapify' s c = heapify (<) minheap_root heap_size c s

max_heapify = flip max_heapify'
min_heapify = flip min_heapify'

heapify :: Rel -> Int -> Int -> IndexedHeap s -> Int -> ST s ()
heapify p o s h i = heapify_l (heap h) o s p i >>= 
                    heapify_r (heap h) o s p i >>= 
		    \largest ->
	    	       if not (largest == i)
		       then swap i largest h >> heapify p o s h largest
		       else return ()

heapify_l :: STUArray s Int Double -> Int -> Int -> Rel -> Int -> ST s Int
heapify_l h o s p i = let l = left (i-(o-1)) + (o-1) in
		      if l > (s + (o-1)) then return i
		      else do elem_l <- readArray h l
		              elem_i <- readArray h i
		              if p elem_l elem_i then return l
		      	      else return i

heapify_r :: STUArray s Int Double -> Int -> Int -> Rel -> Int -> Int -> ST s Int
heapify_r h o s p i largest = do let r = right (i-(o-1)) + (o-1) 
		                 if r > (s + (o-1)) then return largest
			         else do elem_r <- readArray h r
		                         elem_largest <- readArray h largest
		                         if p elem_r elem_largest then return r
			                 else return largest

-- FIXME: rename: push to idx
push_to_root :: Int -> Int -> IndexedHeap s -> ST s ()
push_to_root r i s 
	| i == r = return ()
	| otherwise = let j = parent (i-(r-1)) + (r-1) in
		      swap i j s >> push_to_root r j s

push_to_max_root = push_to_root maxheap_root
push_to_min_root = push_to_root minheap_root 

move_up :: Rel -> Int -> Int -> IndexedHeap s -> ST s Int
move_up r o i s = do let p = parent (i-(o-1)) + (o-1)
	             if (o > p) then return i
		     else do elem_i <- readArray (heap s) i
		             elem_p <- readArray (heap s) p
                             if (r elem_p elem_i) 
		             then swap i p s >> move_up r o p s
		             else return i

move_up_max = move_up (<) maxheap_root
move_up_min = move_up (>) minheap_root

take_median :: IndexedHeap s -> ST s Double
take_median c = readArray (heap c) median

k = 3
median = k+1;
maxheap_root = 1
minheap_root = k+2
heap_size = k
window_size = 2*k+1

init :: Int -> [Double] -> ST s (IndexedHeap s)
init s l = build s l >>= \x -> heapsort (2*s+1) x >>= \y -> reverse' maxheap_root s y 

build :: Int -> [Double] -> ST s (IndexedHeap s)
build s l = liftM3 IndexedHeap heap idx_into_heap idx_into_window
	  where
             heap = newListArray (1, up_idx) l
	     idx_into_heap = newListArray (1, up_idx) [1 .. up_idx]
	     idx_into_window = newListArray (1, up_idx) [1 .. up_idx]
	     up_idx = 2*s+1


reverse' :: Int -> Int -> IndexedHeap s -> ST s (IndexedHeap s)
reverse' i j s 
	| i < j = swap i j s >> reverse' (i+1) (j-1) s
	| otherwise = return s

