module IndexedHeap {-(
	  IndexedHeap(..)
	, build
	, swap
	, max_heapify
	, min_heapify
	, move_up
	, push_to_idx
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
 	    heap :: STUArray s Int Double -- FIXME: rename
 	  , idx_into_heap :: STUArray s Int Int
 	  , idx_into_window :: STUArray s Int Int
 	} 

read_elem :: (?heap :: IndexedHeap s) => Int -> ST s Double
read_elem = readArray (heap ?heap)

write_elem :: (?heap :: IndexedHeap s) => Int -> Double -> ST s ()
write_elem = writeArray (heap ?heap)

read_idx_into_heap :: (?heap :: IndexedHeap s) => Int -> ST s Int
read_idx_into_heap = readArray (idx_into_heap ?heap)

read_idx_into_window :: (?heap :: IndexedHeap s) => Int -> ST s Int
read_idx_into_window = readArray (idx_into_window ?heap)

swap :: (?heap :: IndexedHeap s) => Int -> Int -> ST s ()
swap i j = do -- read values
              heap_elem_i <- readArray (heap ?heap) i
              heap_elem_j <- readArray (heap ?heap) j
	      win_elem_i <- readArray (idx_into_window ?heap) i
	      win_elem_j <- readArray (idx_into_window ?heap) j
	      pos_elem_k <- readArray (idx_into_heap ?heap) win_elem_i
	      pos_elem_l <- readArray (idx_into_heap ?heap) win_elem_j
	      -- update heap
              writeArray (heap ?heap) j heap_elem_i 
              writeArray (heap ?heap) i heap_elem_j 
	      -- update position index
	      writeArray (idx_into_heap ?heap) win_elem_i pos_elem_l
	      writeArray (idx_into_heap ?heap) win_elem_j pos_elem_k
	      -- update window index
	      writeArray (idx_into_window ?heap) j win_elem_i
	      writeArray (idx_into_window ?heap) i win_elem_j

{-# INLINE left #-}
left :: Int -> Int
left i = shiftL i 1

{-# INLINE right #-}
right :: Int -> Int
right i = succ $ shiftL i 1

{-# INLINE parent #-}
parent :: Int -> Int
parent i = shiftR i 1

build_max_heap :: (?heap :: IndexedHeap s) => Int -> ST s ()
build_max_heap size = mapM_ (\i -> heapify (>) maxheap_root size i) $ reverse [1 .. idx]
         where idx = floor (fromIntegral(size)/2) 
	       maxheap_root = 1

heapsort :: (?heap :: IndexedHeap s) => Int -> ST s ()
heapsort 1 = return ()
heapsort n = build_max_heap n >> swap 1 n >> heapsort (n-1)

type Rel = Double -> Double -> Bool

max_heapify :: (?heap :: IndexedHeap s, ?ctx :: Ctx) => Int -> ST s ()
max_heapify s = heapify (>) idx_maxheap_root heap_size s 

min_heapify :: (?heap :: IndexedHeap s, ?ctx :: Ctx) => Int -> ST s ()
min_heapify s = heapify (<) idx_minheap_root heap_size s

heapify :: (?heap :: IndexedHeap s) => Rel -> Int -> Int -> Int -> ST s ()
heapify p o s i = heapify_l (heap ?heap) o s p i >>= 
                    heapify_r (heap ?heap) o s p i >>= 
		    \largest ->
	    	       if not (largest == i)
		       then swap i largest >> heapify p o s largest
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

push_to_idx :: (?heap :: IndexedHeap s) => Int -> Int -> ST s ()
push_to_idx r i 
	| i == r = return ()
	| otherwise = let j = parent (i-(r-1)) + (r-1) in
		      swap i j >> push_to_idx r j

push_to_max_root :: (?heap :: IndexedHeap s, ?ctx :: Ctx) => Int -> ST s ()
push_to_max_root = push_to_idx idx_maxheap_root

push_to_min_root :: (?heap :: IndexedHeap s, ?ctx :: Ctx) => Int -> ST s ()
push_to_min_root = push_to_idx idx_minheap_root 

move_up :: (?heap :: IndexedHeap s) => Rel -> Int -> Int -> ST s Int
move_up r o i = do let p = parent (i-(o-1)) + (o-1)
	           if (o > p) then return i
	           else do elem_i <- readArray (heap ?heap) i
		           elem_p <- readArray (heap ?heap) p
                           if (r elem_p elem_i) 
		           then swap i p >> move_up r o p
		           else return i

move_up_max :: (?heap :: IndexedHeap s, ?ctx :: Ctx) => Int -> ST s Int
move_up_max = move_up (<) idx_maxheap_root

move_up_min :: (?heap :: IndexedHeap s, ?ctx :: Ctx) => Int -> ST s Int
move_up_min = move_up (>) idx_minheap_root

take_median :: (?heap :: IndexedHeap s, ?ctx :: Ctx) => ST s Double
take_median = readArray (heap ?heap) idx_median

data Ctx = C { k' :: Int 
             , idx_median' :: Int
	     , idx_maxheap_root' :: Int
	     , idx_minheap_root' :: Int
	     , heap_size' :: Int
       	     , window_size' :: Int
            }		

buildCtx :: Int -> Ctx
buildCtx k = C k (k+1) 1 (k+2) k (2*k+1)

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

init :: (?ctx :: Ctx) => Int -> [Double] -> ST s (IndexedHeap s)
init s l = do x <- build s l 
	      let ?heap = x
              heapsort (2*s+1) 
	      reverse' idx_maxheap_root s 

reverse' :: (?heap :: IndexedHeap s) => Int -> Int -> ST s (IndexedHeap s)
reverse' i j 
	| i < j = swap i j >> reverse' (i+1) (j-1)
	| otherwise = return ?heap

build :: Int -> [Double] -> ST s (IndexedHeap s)
build s l = liftM3 IndexedHeap heap idx_into_heap idx_into_window
	  where
             heap = newListArray (1, up_idx) l
	     idx_into_heap = newListArray (1, up_idx) [1 .. up_idx]
	     idx_into_window = newListArray (1, up_idx) [1 .. up_idx]
	     up_idx = 2*s+1

