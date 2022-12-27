module Main where

import System.Environment (getArgs)
import Text.Printf (printf)


slice :: [a] -> (Int, Int) -> [a]
slice xs (n, m) = drop n $ take m xs

pairs :: (Ord a) => [a] -> [(a, a)]
pairs xs = [(a, b) | a <- xs, b <- xs, a < b]

existsTarget :: [Int] -> Int -> Bool
existsTarget window target = any (\(a, b) -> a + b == target) $ pairs window

-- Find the first number that is not the sum of a pair of 2 elements of the window
firstInvalid :: Int -> [Int] -> Int
firstInvalid preamble xs =
    let
        ys = take (preamble + 1) xs
        window = init ys
        target = last ys
    in
    if
        not (existsTarget window target) then target
    else
        firstInvalid preamble (tail xs)

-- Find contiguous subset that sums to target
contiguousSubsetSum :: [Int] -> Int -> [Int]
contiguousSubsetSum xs target = subsetSum xs 0 2 target
    where
        subsetSum :: [Int] -> Int -> Int -> Int -> [Int]
        subsetSum xs lo hi target =
            let
                subset = slice xs (lo, hi)
            in
                case compare (sum subset) target of
                    EQ -> subset
                    LT -> subsetSum xs lo (hi+1) target
                    GT -> subsetSum xs (lo+1) hi target


main :: IO ()
main = do
    args <- getArgs -- IO [String]
    content <- readFile (head args) -- IO String
    let xs = map read (lines content) :: [Int]
    let ix = firstInvalid 25 xs -- preamble size == 25
    let cs = contiguousSubsetSum xs ix
    printf "Part 1: %d\n" ix
    printf "Part 2: %d\n" $ minimum cs + maximum cs