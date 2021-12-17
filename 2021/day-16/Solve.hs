module Solve where

import Parser
import Text.Printf (printf)

runParser :: String -> Packet
runParser s = case parseWith packetp s of
    [(pkt, "")] -> pkt
    e -> error (show e)

sumVer :: Packet -> Int
sumVer (V v _ _) = v
sumVer (O v _ ps) = v + sum (map sumVer ps)

eval :: Packet -> Int
eval (V _ _ v) = v
eval (O _ t ps) = case t of
    0 -> sum $ map eval ps
    1 -> product $ map eval ps
    2 -> minimum $ map eval ps
    3 -> maximum $ map eval ps
    5 -> fromEnum $ eval (head ps) > eval (last ps)
    6 -> fromEnum $ eval (head ps) < eval (last ps)
    7 -> fromEnum $ eval (head ps) == eval (last ps)

main :: IO ()
main = do
    xs <- getLine
    let pkt = runParser xs
    printf "Part 1: %d\n" $ sumVer pkt
    printf "Part 2: %d\n" $ eval pkt
