module Main where

import Data.Monoid
import Data.List
import Text.Printf (printf)


sameAdjacent :: (Int -> Bool) -> Int -> Bool
sameAdjacent pred x = any (pred . length) $ (group . show) x

neverDecrease :: Int -> Bool
neverDecrease x = let ds = digits x in sort ds == ds
    where
        digits :: Int -> [Int]
        digits x = [read [a] :: Int | a <- show x]

inRange :: Int -> Int -> Int -> Bool
inRange lo hi x = lo <= x && x <= hi

reducePreds :: [Int -> Bool] -> (Int -> Bool)
reducePreds preds = getAll . foldMap (All .) preds

preds :: [Int -> Bool]
preds = [inRange 271973 785961, neverDecrease]

enumerate :: [Int -> Bool] -> [Int]
enumerate ps = filter (reducePreds ps) [round 1e5 .. round 1e6 - 1]

part1 = enumerate $ preds ++ [sameAdjacent (>= 2)]
part2 = enumerate $ preds ++ [sameAdjacent (== 2)]


main = do
    printf "Part 1: %d\n" $ length part1
    printf "Part 2: %d\n" $ length part2
