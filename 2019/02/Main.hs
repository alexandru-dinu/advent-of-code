module Main where

import System.Environment (getArgs)
import Text.Printf (printf)
import qualified Data.Text.IO as T

import Intcode


part1 :: Memory -> Int
part1 mem = runFromMemory mem (12, 2)

part2 :: Memory -> Int
part2 mem = head [100 * noun + verb | noun <- [0..99], verb <- [0..99], runFromMemory mem (noun, verb) == 19690720]


main :: IO ()
main = do
    file:_ <- getArgs
    mem <- getInitialMemory <$> T.readFile file
    printf "Part 1: %d\n" $ part1 mem
    printf "Part 2: %d\n" $ part2 mem
