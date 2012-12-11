-- Travelling Salesman as found in "Numerical Recipes: The Art of Scientific Computing" 
-- by William H. Press, Saul A. Teukolsky, William T. Vetterling and Brian P. Flannery
import SimulatedAnnealing
import System.Random

newtype Positions = P { yield :: [(Int, Int)] } deriving (Show)

instance SA Positions where
    initial = P initial' 
    neighbour = neighbour'
    objective = objective' . yield

initial' :: [(Int, Int)] 
initial' = [(7,4), (-3,2), (4,2), (10,0), (0,0)]

neighbour' :: RandomGen g => g -> Positions -> (Positions, g) 
neighbour' g p = do let (l, g') = randomR (0, n-1) g
                    let (r, g'') = randomR (l, n-1) g' in (P $ rearrange xs l r , g'')
        where n = length xs
              xs = yield p

rearrange :: [a] -> Int -> Int -> [a]
rearrange xs l r = let (a, b) = splitAt (l-1) xs 
                       (c, d) = splitAt (r-l) b in a ++ reverse c ++ d

objective' :: [(Int, Int)] -> Energy 
objective' xs@(x:xt) = sum $ zipWith dist xs (xt++[x])
	where dist :: (Int, Int) -> (Int, Int) -> Energy
              dist (x_i, y_i) (x_j, y_j) = sqrt(fromIntegral $ (x_i - x_j)^2 + (y_i - y_j)^2)

main :: IO ()
main = let s = (initial :: Positions) in generic_main s
