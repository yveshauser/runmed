-- | Implementation of a running median smoother according to the algorithm described in Haerdle und Steiger (1995).
module RunningMedian (
          runmed
        )
where

import Control.Monad.Reader (asks, runReaderT)
import Control.Monad.ST (runST)
import Control.Monad ((>=>))

import Internal.RunningMedian
import Internal.MonadUtils (mapAccumLM)

-- | Running median filter, i.e., @y_i = median (x_i-k, ..., x_i+k)@, for @k < i < l-k@, where @l = length xs@. The first and the last @k@ elements are given by the 'begin_rule' and 'end_rule'
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
    end_rule k l = let n = (length l) - k in drop n l

runmed' :: Int -> [Double] -> [Double]
runmed' k xs = runST $ build_window env xs >>= run
  where
    env = build_environment k
    ys = drop (window_size env) xs
    run h = runReaderT (runmed'' h ys) env

runmed'' :: Arrays s -> [Double] -> Stack s [Double]
runmed'' h xs = do
  heap <- order h
  y <- peek_median heap
  ys <- mapAccumLM update_and_median heap xs
  return (y:(snd ys))

update_and_median :: Arrays s -> Double -> Stack s (Arrays s, Double)
update_and_median win x_in = (update_heap x_in >=> acc_and_median) win
  where
    update_heap :: Double -> Arrays s -> Stack s (Arrays s)
    update_heap x_in win@(heap, i1, i2, o) =
      do i     <- peek_index win (o+1)
         x_out <- peek_elem win i
         med   <- peek_median win
         update_elem win i x_in
         rebuild_heap win i x_out x_in med
    acc_and_median :: Arrays s -> Stack s (Arrays s, Double)
    acc_and_median win@(heap, i1, i2, o) =
      do med  <- peek_median win
         size <- asks window_size
         return ((heap, i1, i2, mod (o+1) size), med)

rebuild_heap :: Arrays s -> Int -> Double -> Double -> Double -> Stack s (Arrays s)
rebuild_heap win i x_out x_in med
  -- minheap out
  | x_out >  med && x_in >  med = min_out_min_in i win
  | x_out >= med && x_in <= med = min_out_max_in i win
  -- maxheap out
  | x_out <  med && x_in <  med = max_out_max_in i win
  | x_out <= med && x_in >= med = max_out_min_in i win
  -- median out
  | x_out == med && x_in >  med = med_out_min_in i win
  | x_out == med && x_in <  med = med_out_max_in i win
  | x_out == med && x_in == med = med_out_med_in i win
  where
    min_out_min_in i = move_up_min i >=> uncurry min_heapify
    min_out_max_in i = push_to_min_root i >=> swap_minroot_median >=> \win' -> do
      maxroot <- peek_maxroot win'
      if maxroot > x_in
        then (swap_median_maxroot >=> max_heapify_full) win'
        else return win'
    max_out_max_in i = move_up_max i >=> uncurry max_heapify
    max_out_min_in i = push_to_max_root i >=> swap_maxroot_median >=> \win' -> do
      minroot <- peek_minroot win'
      if minroot < x_in
        then (swap_median_minroot >=> min_heapify_full) win'
        else return win'
    med_out_min_in i win = do
      minroot <- peek_minroot win
      if minroot < x_in
        then (swap_index_median i >=> swap_median_minroot >=> min_heapify_full) win
        else return win
    med_out_max_in i win = do
      maxroot <- peek_maxroot win
      if maxroot > x_in
        then (swap_index_median i >=> swap_median_maxroot >=> max_heapify_full) win
        else return win
    med_out_med_in _ win = return win
