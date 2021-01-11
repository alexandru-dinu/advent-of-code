module Main where

import System.Environment (getArgs)
import Text.Printf (printf)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Array as A


type Memory = A.Array Int Int
data Program = Program Int Memory deriving Show


getInitialMemory :: T.Text -> Memory
getInitialMemory txt = A.array (0, length xs - 1) (zip [0..] xs)
    where
        xs = (map (read . T.unpack) . T.splitOn (T.pack ",") . T.strip) txt


update :: Memory -> Int -> (Int -> Int -> Int) -> Memory
update mem ip func = mem A.// [(dst, func op1 op2)]
    where
        dst = mem A.! (ip + 3)
        op1 = mem A.! (mem A.! (ip + 1))
        op2 = mem A.! (mem A.! (ip + 2))


execute :: Program -> Program
execute p@(Program ip mem) = case mem A.! ip of
    1  -> execute (Program (ip + 4) $ update mem ip (+))
    2  -> execute (Program (ip + 4) $ update mem ip (*))
    99 -> p
    _  -> error "Invalid IP."


runFromMemory :: Memory -> (Int, Int) -> Int
runFromMemory mem (noun, verb) = mem' A.! 0
    where
        Program _ mem' = execute $ Program 0 $ mem A.// [(1, noun), (2, verb)]


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
