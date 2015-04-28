{-# LANGUAGE FlexibleContexts #-}

module Internal.RunningMedian
where

import Prelude hiding ( init )
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.ST
import Control.Monad.ST.Class
import Data.Array.IO
import Data.Array.ST
import Data.Bits
import Data.Traversable

type Stack s = ReaderT Env (ST s)
type Array s a = STUArray s Int a
type Arrays s = (Array s Double, Array s Int, Array s Int, Int)

build_window :: Env -> [Double] -> ST s (Arrays s)
build_window ctx l = (,,,) <$> heap <*> idx_into_heap <*> idx_into_window <*> pure 0
    where heap = newListArray (1, up_idx) l
          idx_into_heap = newListArray (1, up_idx) [1 .. up_idx]
          idx_into_window = newListArray (1, up_idx) [1 .. up_idx]
          up_idx = window_size ctx

read_array arr i = liftST $ readArray arr i
write_array arr i e = liftST $ writeArray arr i e

-- | swap two array elements
swap :: Arrays s -> Int -> Int -> Stack s (Arrays s)
swap ind@(heap, i1, i2, _) i j = do
   x <- read_array heap i
   y <- read_array heap j
   k <- read_array i2 i
   l <- read_array i2 j
   write_array heap j x
   write_array heap i y
   write_array i1 k j
   write_array i1 l i
   write_array i2 j k
   write_array i2 i l
   return ind

data Env = Env { heap_size :: Int
               , idx_median :: Int
               , idx_maxheap_root :: Int
               , idx_minheap_root :: Int
               , window_size :: Int
               }

build_environment :: Int -> Env
build_environment k = Env k (k+1) 1 (k+2) (2*k+1)

order :: Arrays s -> Stack s (Arrays s)
order ind = asks window_size >>= heapsort ind >> do
            i <- asks idx_maxheap_root
            j <- asks heap_size
            reverse i j
        where reverse i j
                | i < j = swap ind i j >> reverse (succ i) (pred j)
                | otherwise = return ind

swap_maxroot_median ind = ((,) <$> asks idx_maxheap_root <*> asks idx_median) >>= uncurry (swap ind)
swap_minroot_median ind = ((,) <$> asks idx_minheap_root <*> asks idx_median) >>= uncurry (swap ind)
swap_index_median i ind = ((,) <$> pure i <*> asks idx_median) >>= uncurry (swap ind)
swap_median_maxroot = swap_maxroot_median
swap_median_minroot = swap_minroot_median
swap_median_index = swap_index_median

peek_index (_, i1, _, _) i = read_array i1 i
peek_elem (heap, _, _, _) i = read_array heap i
peek_median (heap, _, _, _) = asks idx_median >>= read_array heap
peek_minroot (heap, _, _, _) = asks idx_minheap_root >>= read_array heap
peek_maxroot (heap, _, _, _) = asks idx_maxheap_root >>= read_array heap

update_elem (heap, _, _, _) i x = write_array heap i x

move_up_max :: Int -> Arrays s -> Stack s (Arrays s, Int)
move_up_max i ind = asks idx_maxheap_root >>= \r -> move_up ind Max r i

move_up_min :: Int -> Arrays s -> Stack s (Arrays s, Int)
move_up_min i ind = asks idx_minheap_root >>= \r -> move_up ind Min r i

max_heapify :: Arrays s -> Int -> Stack s (Arrays s)
max_heapify ind i = do s <- asks heap_size
                       r <- asks idx_maxheap_root
                       heapify ind r Max s i

min_heapify :: Arrays s -> Int -> Stack s (Arrays s)
min_heapify ind i = do s <- asks heap_size
                       r <- asks idx_minheap_root
                       heapify ind r Min s i

max_heapify_full :: Arrays s -> Stack s (Arrays s)
max_heapify_full ind = asks idx_maxheap_root >>= max_heapify ind

min_heapify_full :: Arrays s -> Stack s (Arrays s)
min_heapify_full ind = asks idx_minheap_root >>= min_heapify ind

push_to_max_root i ind = asks idx_maxheap_root >>= \ix -> push_to_idx ind ix i
push_to_min_root i ind = asks idx_minheap_root >>= \ix -> push_to_idx ind ix i

-- Heap operations

{-# INLINE left #-}
left :: Int -> Int
left i = shiftL i 1

{-# INLINE right #-}
right :: Int -> Int
right i = succ $ shiftL i 1

{-# INLINE parent #-}
parent :: Int -> Int
parent i = shiftR i 1

{-# INLINE parent_with_offset #-}
parent_with_offset :: Int -> Int -> Int
parent_with_offset i o = let s = o-1 in parent (i-s) + s

build_max_heap :: Arrays s -> Int -> Stack s ()
build_max_heap ind s = let up_idx = div s 2 in mapM_ (heapify ind 1 Max s) $ reverse [1 .. up_idx]

heapsort :: Arrays s -> Int -> Stack s ()
heapsort ind 1 = return ()
heapsort ind n = build_max_heap ind n >> swap ind 1 n >> heapsort ind (n-1)

data Prio = Min | Max

heapify :: Arrays s -> Int -> Prio -> Int -> Int -> Stack s (Arrays s)
heapify ind ix p s i = do
  l <- heapify_l ind ix p s i
  m <- heapify_r ind ix p s i l
  if m == i then return ind
  else swap ind i m >> heapify ind ix p s m

heapify_l :: Arrays s -> Int -> Prio -> Int -> Int -> Stack s Int
heapify_l (heap, _, _, _) ix Min s i =
  do let o = ix - 1
     let l = left (i-o)
     if l > s then return i
     else idx_of heap (<) (l+o) i

heapify_l (heap, _, _, _) ix Max s i = let l = left i in if l > s then return i else idx_of heap (>) l i

heapify_r :: Arrays s -> Int -> Prio -> Int -> Int -> Int -> Stack s Int
heapify_r (heap, _, _, _) ix Min s i m = do
  let o = ix-1
  let r = right (i-o) in if r > s then return m else idx_of heap (<) (r+o) m

heapify_r (heap, _, _, _) ix Max s i m = let r = right i in if r > s then return m else idx_of heap (>) r m

idx_of :: (Array s Double) -> (Double -> Double -> Bool) -> Int -> Int -> Stack s Int
idx_of arr r i j = cmp arr r i j >>= \cond -> if cond then (return i) else (return j)

cmp :: (Array s Double) -> (Double -> Double -> Bool) -> Int -> Int -> Stack s Bool
cmp arr r i j = lift $ liftM2 r (readArray arr i) (readArray arr j)

push_to_idx ind r i
  | i == r = return ind
  | otherwise = let j = parent_with_offset i r in swap ind i j >>= \a -> push_to_idx a r j

move_up :: Arrays s -> Prio -> Int -> Int -> Stack s (Arrays s, Int)
move_up ind@(heap, _, _, _) Min ix i = do
  let p = parent_with_offset i ix
  if (ix > p) then return (ind, i)
  else do cond <- cmp heap (<) i p
          if cond then swap ind i p >>= \i -> move_up i Min ix p
          else return (ind, i)

move_up ind@(heap, _, _, _) Max ix i = do
  let p = parent i
  if (ix > p) then return (ind, i)
  else do cond <- cmp heap (>) i p
          if cond then swap ind i p >>= \i -> move_up i Max ix p
          else return (ind, i)
