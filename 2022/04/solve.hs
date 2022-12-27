import Data.List
import Data.List.Split (splitOn)
import System.Environment (getArgs)
import Text.Printf (printf)

msplit :: (Eq a) => [[a]] -> [a] -> [[a]]
msplit ds xs = foldl (\acc d -> concatMap (splitOn d) acc) [xs] ds

parseInput :: String -> [Int]
parseInput = map read . msplit [",", "-"]

lteOrdered :: Ord a => [a] -> Bool
lteOrdered xs = xs == sort xs

completeOverlap :: [Int] -> Bool
completeOverlap [a,b,c,d] = any lteOrdered [[a,c,d,b], [c,a,b,d]]

partialOverlap :: [Int] -> Bool
partialOverlap [a,b,c,d] = any lteOrdered [[a,c,b], [c,a,d]]

main :: IO ()
main = do
    args <- getArgs
    content <- readFile $ head args
    let xs = map parseInput $ lines content
    printf "Part 1: %d\n" $ length $ filter completeOverlap xs
    printf "Part 2: %d\n" $ length $ filter partialOverlap xs
