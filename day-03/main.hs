module Main where

import System.Environment (getArgs)
import Text.Printf (printf)

type Path  = String
type Grid  = [Path]
type Coord = (Int, Int)
type Slope = (Int, Int)

slopes :: [Slope]
slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

-- TODO: maybe some folding?
explore :: Grid -> Coord -> Slope -> Path
explore grid (x, y) (dx, dy) = let
    rows = length grid
    cols = length (head grid)
    in
    if y < rows then
        ((grid !! y) !! x) : explore grid ((x + dx) `mod` cols, y + dy) (dx, dy)
    else []

count :: Grid -> Slope -> Int
count grid (dx, dy) = length $ filter (== '#') trail
    where
        trail = explore grid (dx, dy) (dx, dy)


main :: IO ()
main = do
    args <- getArgs -- IO [String]
    grid <- lines <$> readFile (head args) -- IO [String]
    let ans = map (count grid) slopes
    mapM_ print $ zip slopes ans
    printf "Product: %d\n" $ product ans