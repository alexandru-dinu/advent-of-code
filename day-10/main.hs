module Main where

import System.Environment (getArgs)
import Text.Printf (printf)
import Data.List (sort, group)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map


-- Greedily select the next adapter with the min difference
-- (if there are multiple possibilities)
getDifferenceCount :: [Int] -> Map.Map Int Int
getDifferenceCount xs = Map.fromList gs
    where
        ss = [0] ++ sort xs ++ [maximum xs + 3] -- add source (0) and destination (max + 3)
        gs = map (\x -> (head x, length x)) $ (group . sort . map (uncurry (-))) $ zip (tail ss) (init ss)


part1 :: [Int] -> Int
part1 xs = d1 * d3
    where
        counts = getDifferenceCount xs
        d1 = fromMaybe 0 $ Map.lookup 1 counts
        d3 = fromMaybe 0 $ Map.lookup 3 counts


main :: IO ()
main = do
    args <- getArgs -- IO [String]
    content <- readFile (head args) -- IO String
    let xs = map read (lines content) :: [Int]
    printf "Part 1: %d\n" $ part1 xs
    printf "difference counts: %s\n" $ show $ Map.toList $ getDifferenceCount xs