import System.Environment (getArgs)
import Text.Printf (printf)
import Data.List.Split (splitOn)
import qualified Data.Bimap as B

data Move = A | B | C | X | Y | Z deriving (Eq, Read, Show, Enum)

-- | (<=) ltOrdering
ltOrdering :: [(Move, Move)]
ltOrdering = [(A, B), (B, C), (C, A)]

instance Ord Move where
    x <= y = x == y || (x, y) `elem` ltOrdering

valueOf :: Move -> Int
valueOf = (+1) . (`mod` 3) . fromEnum

part1 :: [(Move, Move)] -> Int
part1 = sum . map score
    where
        score :: (Move, Move) -> Int
        score (a, b) = valueOf b + case compare a (eqMove b) of
            LT -> 6
            EQ -> 3
            GT -> 0

        eqMove :: Move -> Move
        eqMove m = toEnum $ fromEnum m `mod` 3

-- X: lose | Y: draw | Z: win
part2 :: [(Move, Move)] -> Int
part2 = sum . map decide
    where
        decide (a, X) = 0 + valueOf (bm B.!> a)
        decide (a, Y) = 3 + valueOf a
        decide (a, Z) = 6 + valueOf (bm B.! a)

        bm :: B.Bimap Move Move
        bm = B.fromList ltOrdering


getInput :: String -> [(Move, Move)]
getInput = map (tuplify . map read . splitOn " ") . lines
    where
        tuplify [x, y] = (x, y)

main :: IO ()
main = do
    args <- getArgs
    content <- readFile $ head args
    let xs = getInput content
    printf "Part 1: %d\n" $ part1 xs
    printf "Part 2: %d\n" $ part2 xs
