import System.Environment (getArgs)
import Text.Printf (printf)
import Data.List.Split (splitOn)


-- | xs[i] += y
updateAt :: (Num a) => Int -> a -> [a] -> [a]
updateAt _ _ [] = []
updateAt 0 y (x:xs) = (x+y):xs
updateAt i y (x:xs) = x:updateAt (i-1) y xs


-- | Decrement timers by rotating left.
-- | All 0's (at the beginning) become 8's (at the end) and increment existing 6's.
step :: [Int] -> [Int]
step [] = []
step (x:xs) = updateAt 6 x $ xs ++ [x]


-- -- | Initial population -> Number of days -> Final population
simulate :: [Int] -> Int -> Int
simulate pop days = (sum . last . take (days + 1) . iterate step) initState
    where
        initState = foldl (\acc i -> updateAt i 1 acc) (replicate 9 0) pop


main :: IO ()
main = do
    args <- getArgs
    content <- readFile $ head args
    let xs = map read $ splitOn "," content :: [Int]
    printf "Part 1: %d\n" $ simulate xs 80
    printf "Part 2: %d\n" $ simulate xs 256
