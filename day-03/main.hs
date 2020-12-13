module Main where

import System.Environment (getArgs)
import Text.Printf (printf)

type Path  = String
type Grid  = [Path]
type Pos   = (Int, Int)
type Slope = (Int, Int)

slopes :: [Slope]
slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]


at :: Grid -> Pos -> Char
at grid (i, j) = (grid !! j) !! i

-- Compute the next position
next :: Slope -> Int -> Pos -> Pos
next (dx, dy) cols (x, y) = ((x + dx) `mod` cols, y + dy)

count :: Grid -> Slope -> Int
count grid slope = length $ filter (== '#') trail
    where
        rows = length grid
        cols = length (head grid)
        -- iterate `next` from (0, 0) to the bottom of the grid
        -- tail because we want to skip (0, 0) from the final list
        positions = tail $ takeWhile (\(x, y) -> y < rows) $ iterate (next slope cols) (0, 0)
        -- get grid chars at those positions
        trail = map (at grid) positions


main :: IO ()
main = do
    args <- getArgs -- IO [String]
    grid <- lines <$> readFile (head args) -- IO [String]
    let ans = map (count grid) slopes
    mapM_ print $ zip slopes ans
    printf "Product: %d\n" $ product ans