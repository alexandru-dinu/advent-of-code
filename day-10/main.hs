module Main where

import System.Environment (getArgs)
import Text.Printf (printf)
import Data.List (sort, partition)


-- Greedily select the next adapter with the min difference
-- (if there are multiple possibilities)
getDifferenceProduct :: [Int] -> Int
getDifferenceProduct xs = d1 * d3
    where
        ss = [0] ++ sort xs ++ [maximum xs + 3] -- add source (0) and destination (max + 3)
        zs = partition (== 1) $ map (uncurry (-)) $ zip (tail ss) (init ss)
        d1 = (length . fst) zs
        d3 = (length . snd) zs


main :: IO ()
main = do
    args <- getArgs -- IO [String]
    content <- readFile (head args) -- IO String
    let xs = map read (lines content) :: [Int]
    printf "Part 1: %d\n" $ getDifferenceProduct xs