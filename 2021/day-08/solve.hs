-- Upsolving, neat idea from
-- https://www.reddit.com/r/adventofcode/comments/rbj87a/2021_day_8_solutions/hnoyy04/

import System.Environment (getArgs)
import Text.Printf (printf)
import Data.List (intersect)
import Data.List.Split (splitOn)


segN :: Int -> [String] -> String
segN n = head . filter (\x -> length x == n)


decodeWith :: String -> String -> String -> Char
decodeWith seg2 seg4 xs = let
    i2 = intersect xs seg2
    i4 = intersect xs seg4
    in case (length xs, length i4, length i2) of
         (2,_,_) -> '1'
         (3,_,_) -> '7'
         (4,_,_) -> '4'
         (7,_,_) -> '8'
         (5,2,_) -> '2'
         (5,3,1) -> '5'
         (5,3,2) -> '3'
         (6,4,_) -> '9'
         (6,3,1) -> '6'
         (6,3,2) -> '0'


-- | Count how many `1,4,7,8` digits are in the output
-- | knowing that they have `2,4,3,7` segments respectively.
part1 :: [String] -> Int
part1 = length . filter (\x -> length x `elem` [2,4,3,7])


-- | Decode output using info from input, then return the constructed 4-digit number
part2 :: [String] -> [String] -> Int
part2 input output = read (map (decodeWith seg2 seg4) output) :: Int
    where
        seg2 = segN 2 input
        seg4 = segN 4 input


main :: IO ()
main = do
    args <- getArgs
    content <- readFile $ head args
    let xs = map (map (splitOn " ") . splitOn " | ") $ lines content -- :: [ [[input], [output]] ]
    printf "Part 1: %d\n" $ sum $ map (\x -> part1          (last x)) xs
    printf "Part 2: %d\n" $ sum $ map (\x -> part2 (head x) (last x)) xs
