module Internal.MonadUtils (
    mapAccumLM
  )
where

mapAccumLM :: Monad m => (acc -> x -> m (acc, y)) -> acc -> [x] -> m (acc, [y])
mapAccumLM _ s []     = return (s, [])
mapAccumLM f s (x:xs) = do
  (s1, x')  <- f s x
  (s2, xs') <- mapAccumLM f s1 xs
  return (s2, x': xs')
