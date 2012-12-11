-- Simulated Annealing algorithm as described in "Numerical Recipes: The Art of Scientific Computing" 
-- by William H. Press, Saul A. Teukolsky, William T. Vetterling and Brian P. Flannery
module SimulatedAnnealing 
    (
      SA (..)
    , Energy
    , generic_main
    )
    where

import Control.Monad.State
import System.Random

-- | Interface
class Show a => SA a where
    initial :: a
    neighbour :: RandomGen g => g -> a -> (a, g)
    objective :: a -> Double

-- | Type defs
type Energy = Double
type Temperature = Double

-- | Temperature
temperature :: Double -> Double
temperature x = 1 - x

-- | Acceptance probability
acceptance_prob :: Energy -> Energy -> Temperature -> Double
acceptance_prob e1 e2 t = exp(-(e2-e1)/(boltzmann*t))

-- | Global params
boltzmann = 1.3
kmax = 1000
emax = 0.1

-- | run
run :: (SA a, RandomGen g) => (a, Energy, g) -> Double -> StateT (a, Energy) IO (a, Energy, g)
run p@(_, e, g) _ | e < emax = return p
run (s, e, g) k | otherwise = 
    do (sbest, ebest) <- get
       let t = temperature $ k/kmax
       let (snew, g') = neighbour g s
       let enew = objective snew 
       let (r, g'') = randomR (0, 1) g'
       let (a, b) = if (acceptance_prob e enew t > r) then (snew, enew) else (s, e)
       if e < ebest then put (snew, enew) >> return (a, b, g'') else return (a, b, g'')

-- | generic main function
generic_main :: SA a => a -> IO ()
generic_main s = newStdGen >>= \g -> runStateT (go s g) (s, objective s) >>= putStr . show . snd
    where go s g = foldM run (init' s g) [1..kmax]
          init' s g = (s, objective s, g)
