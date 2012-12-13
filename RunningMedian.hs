{-# LANGUAGE ImplicitParams #-}

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
import Control.Monad.ST
import Data.Array.ST
import Data.Bits

import qualified Data.Vector.Unboxed as V

type Vector = V.Vector Double

-- Note: Using implicit parameters language extension as motivated in
--       Functional Pearls: Global Variables in Haskell, John Hughes 2004

-- | Running median filter, i.e., @y_i = median (x_i-k, ..., x_i+k)@, for @k < i < l-k@, where @l = length xs@. The first and the last @k@ elements are given by the 'begin_rule' and 'end_rule'
--
--   The algorithm is running in optimal time, see referenced paper.
runmed :: Int       -- ^ The size @k@, where @2*k+1@ is the window size
       -> Vector    -- ^ The input vector @xs@
       -> Vector    -- ^ The output vector @ys@
runmed k xs = let ?ctx = buildCtx k in runmed' xs

runmed' :: (?ctx :: Ctx) => Vector -> Vector
runmed' xs
   | V.length xs < window_size = xs
   | otherwise = let k = heap_size in begin_rule k xs `cat` runmed'' xs `cat` end_rule k xs
	where cat = (V.++)

runmed'' :: (?ctx :: Ctx) => Vector -> Vector
runmed'' l = runST $ do i <- buildInd l
                        let ?ind = i
                        init
                        liftM2 V.cons take_median $ imapM step xs
    where xs = V.drop window_size l
          imapM :: Monad m => (Int -> Double -> m Double) -> Vector -> m Vector
          imapM f v = let n = V.length v in V.mapM (uncurry f) $ V.zip (V.fromList [0..n-1]) v

begin_rule :: Int -> Vector -> Vector
begin_rule = V.take

end_rule :: Int -> Vector -> Vector
end_rule k l = let n = (V.length l) - k in V.drop n l

step :: (?ind :: Indexed s, ?ctx :: Ctx) => Int -> Double -> ST s Double
step o x_in = do i <- read_idx_into_elems $ (mod o window_size)+1
                 x_out <- read_elem i
                 med <- read_elem idx_median
                 write_elem i x_in
                 rebuild_heap i x_out x_in med
                 take_median

rebuild_heap :: (?ind :: Indexed s, ?ctx :: Ctx) => Int -> Double -> Double -> Double -> ST s ()
rebuild_heap i x_out x_in med
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
        max_out_max_in i = move_up_max i >>= max_heapify
        min_out_min_in i = move_up_min i >>= min_heapify
        max_out_min_in i = push_to_max_root i >> swap idx_maxheap_root idx_median >>
                           do min_root <- read_elem idx_minheap_root
                              if min_root < x_in then swap idx_median idx_minheap_root >> min_heapify idx_minheap_root
                              else return ()
        min_out_max_in i = push_to_min_root i >> swap idx_minheap_root idx_median >>
                           do max_root <- read_elem idx_maxheap_root
                              if max_root > x_in then swap idx_median idx_maxheap_root >> max_heapify idx_maxheap_root
                              else return ()
        med_out_min_in i = do min_root <- read_elem idx_minheap_root
                              if min_root < x_in then swap i idx_median >> swap idx_median idx_minheap_root >> min_heapify idx_minheap_root
                              else return ()
        med_out_max_in i = do max_root <- read_elem idx_maxheap_root
                              if max_root > x_in then swap i idx_median >> swap idx_median idx_maxheap_root >> max_heapify idx_maxheap_root
                              else return ()
        med_out_med_in _ = return ()

-- Data structure

data Indexed s = Indexed {
            elems :: STUArray s Int Double
          , idx_into_elems :: STUArray s Int Int
          , idx_into_window :: STUArray s Int Int
        }

{-# INLINE read_elem #-}
read_elem :: (?ind :: Indexed s) => Int -> ST s Double
read_elem = readArray (elems ?ind)

{-# INLINE write_elem #-}
write_elem :: (?ind :: Indexed s) => Int -> Double -> ST s ()
write_elem = writeArray (elems ?ind)

{-# INLINE read_idx_into_elems #-}
read_idx_into_elems :: (?ind :: Indexed s) => Int -> ST s Int
read_idx_into_elems = readArray (idx_into_elems ?ind)

{-# INLINE write_idx_into_elems #-}
write_idx_into_elems :: (?ind :: Indexed s) => Int -> Int -> ST s ()
write_idx_into_elems = writeArray (idx_into_elems ?ind)

{-# INLINE read_idx_into_window #-}
read_idx_into_window :: (?ind :: Indexed s) => Int -> ST s Int
read_idx_into_window = readArray (idx_into_window ?ind)

{-# INLINE write_idx_into_window #-}
write_idx_into_window :: (?ind :: Indexed s) => Int -> Int -> ST s ()
write_idx_into_window = writeArray (idx_into_window ?ind)

-- | swap two elements
swap :: (?ind :: Indexed s) => Int -> Int -> ST s ()
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

-- Having a context as implicit parameter, in order to statically
-- get the window size and dependent parameters

data Ctx = C { heap_size' :: {-# UNPACK #-} !Int
             , idx_median' :: {-# UNPACK #-} !Int
             , idx_maxheap_root' :: {-# UNPACK #-} !Int
             , idx_minheap_root' :: {-# UNPACK #-} !Int
             , window_size' :: {-# UNPACK #-} !Int
            }

buildCtx :: Int -> Ctx
buildCtx k = C k (k+1) 1 (k+2) (2*k+1)

{-# INLINE idx_median #-}
idx_median :: (?ctx :: Ctx) => Int
idx_median = idx_median' ?ctx

{-# INLINE idx_maxheap_root #-}
idx_maxheap_root :: (?ctx :: Ctx) => Int
idx_maxheap_root = idx_maxheap_root' ?ctx

{-# INLINE idx_minheap_root #-}
idx_minheap_root :: (?ctx :: Ctx) => Int
idx_minheap_root = idx_minheap_root' ?ctx

{-# INLINE heap_size #-}
heap_size :: (?ctx :: Ctx) => Int
heap_size = heap_size' ?ctx

{-# INLINE window_size #-}
window_size :: (?ctx :: Ctx) => Int
window_size = window_size' ?ctx

{-# INLINE take_median #-}
take_median :: (?ind :: Indexed s, ?ctx :: Ctx) => ST s Double
take_median = readArray (elems ?ind) idx_median

-- Construction of the data structure

init :: (?ind :: Indexed s, ?ctx :: Ctx) => ST s ()
init = heapsort window_size >> reverse idx_maxheap_root heap_size
        where reverse i j
                | i < j = swap i j >> reverse (succ i) (pred j)
                | otherwise = return ()

buildInd :: (?ctx :: Ctx) => Vector -> ST s (Indexed s)
buildInd l = liftM3 Indexed heap idx_into_heap idx_into_window
    where heap = newListArray (1, up_idx) $ V.toList l
          idx_into_heap = newListArray (1, up_idx) [1 .. up_idx]
          idx_into_window = newListArray (1, up_idx) [1 .. up_idx]
          up_idx = window_size

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

build_max_heap :: (?ind :: Indexed s, ?ctx :: Ctx) => Int -> ST s ()
build_max_heap s = let up_idx = div s 2 in mapM_ (heapify Max s) $ reverse [1 .. up_idx]

heapsort :: (?ind :: Indexed s, ?ctx :: Ctx) => Int -> ST s ()
heapsort 1 = return ()
heapsort n = build_max_heap n >> swap 1 n >> heapsort (n-1)

data Prio = Min | Max

max_heapify :: (?ind :: Indexed s, ?ctx :: Ctx) => Int -> ST s ()
max_heapify = heapify Max heap_size

min_heapify :: (?ind :: Indexed s, ?ctx :: Ctx) => Int -> ST s ()
min_heapify = heapify Min heap_size

heapify :: (?ind :: Indexed s, ?ctx :: Ctx) => Prio -> Int -> Int -> ST s ()
heapify p s i = do l <- heapify_l p s i
                   m <- heapify_r p s i l
                   if m == i then return ()
                   else swap i m >> heapify p s m

heapify_l :: (?ind :: Indexed s, ?ctx :: Ctx) => Prio -> Int -> Int -> ST s Int
heapify_l Min s i = if l > s then return i
                    else idx_of (<) (l+o) i
                where l = left (i-o)
                      o = idx_minheap_root-1

heapify_l Max s i = let l = left i in
                    if l > s then return i
                    else idx_of (>) l i

heapify_r :: (?ind :: Indexed s, ?ctx :: Ctx) => Prio -> Int -> Int -> Int -> ST s Int
heapify_r Min s i m = if r > s then return m
                      else idx_of (<) (r+o) m
                where r = right (i-o)
                      o = idx_minheap_root-1

heapify_r Max s i m = let r = right i in
                      if r > s then return m
                      else idx_of (>) r m

idx_of :: (?ind :: Indexed s) => (Double -> Double -> Bool) -> Int -> Int -> ST s Int
idx_of r i j = cmp r i j >>= \cond -> if cond then (return i) else (return j)

cmp :: (?ind ::  Indexed s) => (Double -> Double -> Bool) -> Int -> Int -> ST s Bool
cmp r i j = liftM2 r (read_elem i) (read_elem j)

push_to_max_root :: (?ind :: Indexed s, ?ctx :: Ctx) => Int -> ST s ()
push_to_max_root = push_to_idx idx_maxheap_root

push_to_min_root :: (?ind :: Indexed s, ?ctx :: Ctx) => Int -> ST s ()
push_to_min_root = push_to_idx idx_minheap_root

push_to_idx :: (?ind :: Indexed s) => Int -> Int -> ST s ()
push_to_idx r i
        | i == r = return ()
        | otherwise = let j = parent_with_offset i r in
                      swap i j >> push_to_idx r j

move_up_max :: (?ind :: Indexed s, ?ctx :: Ctx) => Int -> ST s Int
move_up_max = move_up Max

move_up_min :: (?ind :: Indexed s, ?ctx :: Ctx) => Int -> ST s Int
move_up_min = move_up Min

move_up :: (?ind :: Indexed s, ?ctx :: Ctx) => Prio -> Int -> ST s Int
move_up Min i = let o = idx_minheap_root 
                    p = parent_with_offset i o in
                if (o > p) then return i
                else do cond <- cmp (<) i p
                        if cond then swap i p >> move_up Min p
                        else return i

move_up Max i = let o = idx_maxheap_root 
                    p = parent i in
                if (o > p) then return i
                else do cond <- cmp (>) i p
                        if cond then swap i p >> move_up Max p
                        else return i
