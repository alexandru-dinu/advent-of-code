module Intcode where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Array as A


type Memory = A.Array Int Int
data Program = Program Int Memory deriving Show


getInitialMemory :: T.Text -> Memory
getInitialMemory txt = A.array (0, length xs - 1) (zip [0..] xs)
    where
        xs = (map (read . T.unpack) . T.splitOn (T.pack ",") . T.strip) txt


step2 :: (Int -> Int -> Int) -> Program -> Program
step2 func (Program ip mem) = Program (ip + 4) mem'
    where
        mem' = mem A.// [(dst, func op1 op2)]
        op1  = mem A.! (mem A.! (ip + 1))
        op2  = mem A.! (mem A.! (ip + 2))
        dst  = mem A.! (ip + 3)


execute :: Program -> Program
execute p@(Program ip mem) = case mem A.! ip of
    1  -> execute $ step2 (+) p
    2  -> execute $ step2 (*) p
    99 -> p -- halting state
    _  -> error "Invalid IP."


runFromMemory :: Memory -> (Int, Int) -> Int
runFromMemory mem (noun, verb) = mem' A.! 0
    where
        Program _ mem' = execute $ Program 0 $ mem A.// [(1, noun), (2, verb)]
