-- | Implementation of a running median smoother according to the algorithm described in Haerdle und Steiger (1995).
module RunningMedian (
  runmed
)
where

import Control.Monad.Reader (asks, runReaderT)
import Control.Monad.ST (runST)
import Control.Monad ((>=>))
import RunningMedian.Internal
import RunningMedian.MonadUtils (mapAccumLM)

-- | Running median filter, i.e., @y_i = median (x_i-k, ..., x_i+k)@, for @k < i < l-k@,
--   where @l = length xs@. The first and the last @k@ elements are given by the 'begin_rule' and 'end_rule'
--
--   The algorithm is running in optimal time, see referenced paper.
runmed :: Int       -- ^ The size @k@, where @2*k+1@ is the window size
       -> [Double]  -- ^ The inputs @xs@
       -> [Double]  -- ^ The outputs @ys@
runmed k xs
   | length xs < n = xs
   | otherwise = begin_rule k xs ++ runmed' k xs ++ end_rule k xs
  where
    n = 2*k+1
    begin_rule = take
    end_rule k l = let n = length l - k in drop n l

runmed' :: Int -> [Double] -> [Double]
runmed' k xs = runST $ build_window env xs >>= run
  where
    env = build_environment k
    ys = drop (window_size env) xs
    run h = runReaderT (runmed'' h ys) env

runmed'' :: Arrays s -> [Double] -> Stack s [Double]
runmed'' h xs = do
  heap <- order h
  y    <- peek_median heap
  ys   <- mapAccumLM (flip update_and_median) heap xs
  return (y:snd ys)

update_and_median :: Double -> Arrays s -> Stack s (Arrays s, Double)
update_and_median x_in = update_heap x_in >=> acc_and_median
  where
    update_heap :: Double -> Arrays s -> Stack s (Arrays s)
    update_heap x_in win@(heap, i1, i2, o) =
      do i       <- peek_index win (o+1)
         med     <- peek_median win
         med_idx <- asks idx_median
         update_elem win i x_in
         rebuild_heap i med_idx x_in med win
    acc_and_median :: Arrays s -> Stack s (Arrays s, Double)
    acc_and_median win@(heap, i1, i2, o) =
      do med  <- peek_median win
         size <- asks window_size
         return ((heap, i1, i2, mod (o+1) size), med)

rebuild_heap :: Int -> Int -> Double -> Double -> Arrays s -> Stack s (Arrays s)
rebuild_heap i med_idx x_in med
  -- minheap out
  | i >  med_idx && x_in >  med = min_out_min_in i
  | i >  med_idx && x_in <= med = min_out_max_in i
  -- maxheap out
  | i <  med_idx && x_in <  med = max_out_max_in i
  | i <  med_idx && x_in >= med = max_out_min_in i
  -- median out
  | i == med_idx && x_in >  med = med_out_min_in i
  | i == med_idx && x_in <  med = med_out_max_in i
  | i == med_idx && x_in == med = med_out_med_in i
  where
    min_out_min_in i = move_up_min i >=> min_heapify
    min_out_max_in i = push_to_min_root i >=> swap_minroot_median >=> \win' -> do
      maxroot <- peek_maxroot win'
      if maxroot > x_in
        then (swap_median_maxroot >=> max_heapify_full) win'
        else return win'
    max_out_max_in i = move_up_max i >=> max_heapify
    max_out_min_in i = push_to_max_root i >=> swap_maxroot_median >=> \win' -> do
      minroot <- peek_minroot win'
      if minroot < x_in
        then (swap_median_minroot >=> min_heapify_full) win'
        else return win'
    med_out_min_in i win = do
      minroot <- peek_minroot win
      if minroot < x_in
        then (swap_median_minroot >=> min_heapify_full) win
        else return win
    med_out_max_in i win = do
      maxroot <- peek_maxroot win
      if maxroot > x_in
        then (swap_median_maxroot >=> max_heapify_full) win
        else return win
    med_out_med_in _ = return
