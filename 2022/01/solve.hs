import System.Environment (getArgs)
import Text.Printf (printf)
import Data.List (sort)
import Data.List.Split (splitOn)

part1 :: [Int] -> Int
part1 = maximum

part2 :: [Int] -> Int
part2 = sum . take 3 . reverse . sort

getInput :: String -> [Int]
getInput = map (sum . map read . lines) . splitOn "\n\n"

main :: IO ()
main = do
    args <- getArgs
    content <- readFile $ head args
    let xs = getInput content
    printf "Part 1: %d\n" $ part1 xs
    printf "Part 2: %d\n" $ part2 xs

