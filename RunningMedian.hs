{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE DatatypeContexts #-}
-- {-# LANGUAGE ConstraintKinds #-}

-- | Implementation of a running median smoother according to the
-- algorithm described in Haerdle und Steiger (1995).
module RunningMedian (
          runmed
        , begin_rule
        , end_rule
        )
where

import Prelude hiding ( init )
import Control.Monad
import Data.Array.IO
import Data.Bits

import System.IO.Unsafe

import qualified Data.Vector.Unboxed as V

import GHC.Exts (Constraint)

type Vector = V.Vector Double

-- Note: Using implicit parameters language extension as motivated in
--       Functional Pearls: Global Variables in Haskell, John Hughes 2004

-- | Running median filter, i.e., @y_i = median (x_i-k, ..., x_i+k)@, for @k < i < l-k@, where @l = length xs@. The first and the last @k@ elements are given by the 'begin_rule' and 'end_rule'
--
--   The algorithm is running in optimal time, see referenced paper.
runmed :: Int       -- ^ The size @k@, where @2*k+1@ is the window size
       -> Vector    -- ^ The input vector @xs@
       -> Vector    -- ^ The output vector @ys@
runmed k xs = let ctx = buildCtx k in runmed' ctx xs

runmed' :: Ctx -> Vector -> Vector
runmed' ctx xs
   | V.length xs < (window_size ctx) = xs
   | otherwise = let k = (heap_size ctx) in begin_rule k xs `cat` runmed'' ctx xs `cat` end_rule k xs
  where cat = (V.++)

runmed'' :: Ctx -> Vector -> Vector
runmed'' ctx l = unsafePerformIO $ 
  do moving_window <- buildInd ctx l
     order moving_window ctx
     liftM2 V.cons (readArray (elems moving_window) (idx_median ctx)) $ imapM (step moving_window ctx) xs
    where xs = V.drop (window_size ctx) l
          imapM :: Monad m => (Int -> Double -> m Double) -> Vector -> m Vector
          imapM f v = let n = V.length v in V.mapM (uncurry f) $ V.zip (V.fromList [0..n-1]) v

begin_rule :: Int -> Vector -> Vector
begin_rule = V.take

end_rule :: Int -> Vector -> Vector
end_rule k l = let n = (V.length l) - k in V.drop n l

step :: (MArray a Int m, MArray a Double m) => Indexed3 a -> Ctx -> Int -> Double -> m Double
step ind ctx o x_in =
              do i <- readArray (idx_into_elems ind) $ (mod o (window_size ctx))+1
                 x_out <- readArray (elems ind) i
                 med <- readArray (elems ind) (idx_median ctx)
                 writeArray (elems ind) i x_in
                 rebuild_heap ind ctx i x_out x_in med
                 readArray (elems ind) (idx_median ctx)


-- TODO: typeclass with min_out_min_in etc.?

rebuild_heap :: (MArray a Int m, MArray a Double m) => Indexed3 a -> Ctx -> Int -> Double -> Double -> Double -> m ()
rebuild_heap ind ctx i x_out x_in med
        -- min out
        | x_out > med && x_in >= med = min_out_min_in i
        | x_out > med && x_in <  med = min_out_max_in i
        -- max out
        | x_out < med && x_in <= med = max_out_max_in i
        | x_out < med && x_in >  med = max_out_min_in i
        -- median out
        | x_out == med && x_in >  med = med_out_min_in i
        | x_out == med && x_in <  med = med_out_max_in i
        | x_out == med && x_in == med = med_out_med_in i
  where
        max_out_max_in i = move_up_max ind ctx i >>= max_heapify ind ctx
        min_out_min_in i = move_up_min ind ctx i >>= min_heapify ind ctx
        max_out_min_in i = push_to_max_root ind ctx i >> swap ind (idx_maxheap_root ctx) (idx_median ctx) >>
                           do min_root <- readArray (elems ind) (idx_minheap_root ctx)
                              if min_root < x_in then swap ind (idx_median ctx) (idx_minheap_root ctx) >> min_heapify ind ctx (idx_minheap_root ctx)
                              else return ()
        min_out_max_in i = push_to_min_root ind ctx i >> swap ind (idx_minheap_root ctx) (idx_median ctx) >>
                           do max_root <- readArray (elems ind) (idx_maxheap_root ctx)
                              if max_root > x_in then swap ind (idx_median ctx) (idx_maxheap_root ctx) >> max_heapify ind ctx (idx_maxheap_root ctx)
                              else return ()
        med_out_min_in i = do min_root <- readArray (elems ind) (idx_minheap_root ctx)
                              if min_root < x_in then swap ind i (idx_median ctx) >> swap ind (idx_median ctx) (idx_minheap_root ctx) >> min_heapify ind ctx (idx_minheap_root ctx)
                              else return ()
        med_out_max_in i = do max_root <- readArray (elems ind) (idx_maxheap_root ctx)
                              if max_root > x_in then swap ind i (idx_median ctx) >> swap ind (idx_median ctx) (idx_maxheap_root ctx) >> max_heapify ind ctx (idx_maxheap_root ctx)
                              else return ()
        med_out_med_in _ = return ()

-- Data structure
data Indexed3 a = Indexed3 {
            elems :: a Int Double
          , idx_into_elems :: a Int Int
          , idx_into_window :: a Int Int
        }


-- | swap two elements
swap :: (MArray a Int m, MArray a Double m) => Indexed3 a -> Int -> Int -> m ()
swap ind i j = do -- read values
              heap_elem_i <- readArray (elems ind) i
              heap_elem_j <- readArray (elems ind) j
              win_elem_i <- readArray (idx_into_window ind) i
              win_elem_j <- readArray (idx_into_window ind) j
              pos_elem_k <- readArray (idx_into_elems ind) win_elem_i
              pos_elem_l <- readArray (idx_into_elems ind) win_elem_j
              -- update heap
              writeArray (elems ind) j heap_elem_i
              writeArray (elems ind) i heap_elem_j
              -- update position index
              writeArray (idx_into_elems ind) win_elem_i pos_elem_l
              writeArray (idx_into_elems ind) win_elem_j pos_elem_k
              -- update window index
              writeArray (idx_into_window ind) j win_elem_i
              writeArray (idx_into_window ind) i win_elem_j

-- Having a context as implicit parameter, in order to statically
-- get the window size and dependent parameters

data Ctx = C { heap_size :: {-# UNPACK #-} !Int
             , idx_median :: {-# UNPACK #-} !Int
             , idx_maxheap_root :: {-# UNPACK #-} !Int
             , idx_minheap_root :: {-# UNPACK #-} !Int
             , window_size :: {-# UNPACK #-} !Int
            }

buildCtx :: Int -> Ctx
buildCtx k = C k (k+1) 1 (k+2) (2*k+1)

-- Construction of the data structure

order  :: (MArray a Double m, MArray a Int m) => Indexed3 a -> Ctx -> m ()
order ind ctx = heapsort ind ctx (window_size ctx) >> reverse (idx_maxheap_root ctx) (heap_size ctx)
        where reverse i j
                | i < j = swap ind i j >> reverse (succ i) (pred j)
                | otherwise = return ()
              k = heap_size ctx

buildInd :: Ctx -> Vector -> IO (Indexed3 IOUArray)
buildInd ctx l = liftM3 Indexed3 heap idx_into_heap idx_into_window
    where heap = newListArray (1, up_idx) $ V.toList l
          idx_into_heap = newListArray (1, up_idx) [1 .. (up_idx)]
          idx_into_window = newListArray (1, up_idx) [1 .. (up_idx)]
          up_idx = (window_size ctx)

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

build_max_heap :: (MArray a Int m, MArray a Double m) => Indexed3 a -> Ctx -> Int -> m ()
build_max_heap ind ctx s = let up_idx = div s 2 in mapM_ (heapify ind ctx Max s) $ reverse [1 .. up_idx]

heapsort :: (MArray a Int m, MArray a Double m) => Indexed3 a -> Ctx -> Int -> m ()
heapsort ind ctx 1 = return ()
heapsort ind ctx n = build_max_heap ind ctx n >> swap ind 1 n >> heapsort ind ctx (n-1)

data Prio = Min | Max

max_heapify :: (MArray a Int m, MArray a Double m) => Indexed3 a -> Ctx -> Int -> m ()
max_heapify ind ctx = heapify ind ctx Max (heap_size ctx)

min_heapify :: (MArray a Int m, MArray a Double m) => Indexed3 a -> Ctx -> Int -> m ()
min_heapify ind ctx = heapify ind ctx Min (heap_size ctx)

heapify :: (MArray a Int m, MArray a Double m) => Indexed3 a -> Ctx -> Prio -> Int -> Int -> m ()
heapify ind ctx p s i = 
                do l <- heapify_l (elems ind) ctx p s i
                   m <- heapify_r (elems ind) ctx p s i l
                   if m == i then return ()
                   else swap ind i m >> heapify ind ctx p s m

heapify_l :: (Ord e, MArray a e m) =>
  a Int e -> Ctx -> Prio -> Int -> Int -> m Int

heapify_l arr ctx Min s i = 
                    if l > s then return i
                    else idx_of arr (<) (l+o) i
                where l = left (i-o)
                      o = (idx_minheap_root ctx)-1

heapify_l arr ctx Max s i = 
                    let l = left i in
                    if l > s then return i
                    else idx_of arr (>) l i

heapify_r :: (Ord e, MArray a e m) =>
  a Int e -> Ctx -> Prio -> Int -> Int -> Int -> m Int
heapify_r arr ctx Min s i m = 
                      if r > s then return m
                      else idx_of arr (<) (r+o) m
                where r = right (i-o)
                      o = (idx_minheap_root ctx)-1

heapify_r arr ctx Max s i m = 
                      let r = right i in
                      if r > s then return m
                      else idx_of arr (>) r m

idx_of :: (Ix i, MArray a e m) =>
  a i e -> (e -> e -> Bool) -> i -> i -> m i
idx_of arr r i j = cmp arr r i j >>= \cond -> if cond then (return i) else (return j)

cmp :: (Ix i, MArray a e m) =>
  a i e -> (e -> e -> r) -> i -> i -> m r
cmp arr r i j = liftM2 r (readArray arr i) (readArray arr j)

push_to_max_root :: (MArray a Int m, MArray a Double m) => Indexed3 a -> Ctx -> Int -> m ()
push_to_max_root ind c = push_to_idx ind (idx_maxheap_root c)

push_to_min_root :: (MArray a Int m, MArray a Double m) => Indexed3 a -> Ctx -> Int -> m ()
push_to_min_root ind c = push_to_idx ind (idx_minheap_root c)

push_to_idx :: (MArray a Int m, MArray a Double m) => Indexed3 a -> Int -> Int -> m ()
push_to_idx idx r i
        | i == r = return ()
        | otherwise = let j = parent_with_offset i r in
                      swap idx i j >> push_to_idx idx r j

move_up_max :: (MArray a Int m, MArray a Double m) => Indexed3 a -> Ctx -> Int -> m Int
move_up_max i c = move_up i c Max

move_up_min :: (MArray a Int m, MArray a Double m) => Indexed3 a -> Ctx -> Int -> m Int
move_up_min i c = move_up i c Min

move_up :: (MArray a Int m, MArray a Double m) => Indexed3 a -> Ctx -> Prio -> Int -> m Int
move_up idx c Min i = let o = idx_minheap_root c
                          p = parent_with_offset i o in
                      if (o > p) then return i
                      else do cond <- cmp (elems idx) (<) i p
                              if cond then swap idx i p >> move_up idx c Min p
                              else return i

move_up idx c Max i = let o = idx_maxheap_root c
                          p = parent i in
                      if (o > p) then return i
                      else do cond <- cmp (elems idx) (>) i p
                              if cond then swap idx i p >> move_up idx c Max p
                              else return i
