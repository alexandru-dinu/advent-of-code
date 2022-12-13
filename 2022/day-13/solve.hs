import System.Environment (getArgs)
import Data.Char (isDigit)
import Data.List (sortBy)
import Data.List.Split (splitOn, chunksOf)
import Text.Printf (printf)

data P = I Int | L [P] deriving (Show, Eq, Read)

-- | Make input string parseable by default `read`
prepareP :: String -> String
prepareP [] = []
prepareP ('[':xs) = "L [" ++ prepareP xs
prepareP xs = case span isDigit xs of
    ([], x:rest) -> x : prepareP rest
    (ds, rest) -> ("I " ++ ds) ++ prepareP rest

parseP :: String -> P
parseP = read . prepareP

compareP :: P -> P -> Ordering
compareP (I x) (I y) = compare x y
compareP (L xs) (L ys) = case (xs, ys) of
    ([], []) -> EQ
    ([], _) -> LT
    (_, []) -> GT
    (x:xs', y:ys') -> compareP x y <> compareP (L xs') (L ys')
compareP l@(I _) r@(L _) = compareP (L [l]) r
compareP l@(L _) r@(I _) = compareP l (L [r])

parseInput :: String -> [P]
parseInput = map parseP . concatMap lines . splitOn "\n\n"

tuplify :: [a] -> [(a, a)]
tuplify xs = map t $ chunksOf 2 xs
    where t [x, y] = (x, y)

part1 :: [P] -> Int
part1 ps = sum ltIndices
    where
        ltIndices = map fst $ filter ((== LT) . snd) $ zip [1..] orderings
        orderings = map (uncurry compareP) $ tuplify ps

index :: [P] -> P -> Int
index [] _ = error "Not found"
index (x:xs) q
    | x == q = 1
    | otherwise = 1 + index xs q

dividers :: [P]
dividers = map parseP ["[[2]]", "[[6]]"]

part2 :: [P] -> Int
part2 ps = product $ map (index sorted) dividers
    where
        sorted = sortBy compareP (ps ++ dividers)

main :: IO ()
main = do
    args <- getArgs
    content <- readFile $ head args
    let xs = parseInput content
    printf "Part 1: %d\n" $ part1 xs
    printf "Part 2: %d\n" $ part2 xs
