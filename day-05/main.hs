module Main where

import System.Environment (getArgs)
import Text.Printf (printf)
import Data.Char (digitToInt)
import Data.List (foldl', sort)
import Data.Bits (xor)


toDecimal :: String -> Int
toDecimal = foldl' (\acc x -> acc * 2 + digitToInt x) 0

toBinary :: Char -> Char
toBinary 'F' = '0'
toBinary 'L' = '0'
toBinary 'B' = '1'
toBinary 'R' = '1'

decode :: String -> Int
decode s = row * 8 + col
    where
        b = map toBinary s
        row = toDecimal $ take 7 b
        col = toDecimal $ drop 7 b

findMissing :: [Int] -> Int
findMissing xs = xor x1 x2
    where
        x1 = foldl xor 0 xs
        x2 = foldl xor 0 [minimum xs .. maximum xs]


main :: IO ()
main = do
    args <- getArgs -- IO [String]
    xs <- lines <$> readFile (head args) -- IO [String]
    let ds = map decode xs
    printf "Part 1: %d\n" $ maximum ds
    printf "Part 2: %d\n" $ findMissing ds