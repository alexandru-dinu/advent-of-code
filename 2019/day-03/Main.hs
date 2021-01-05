module Main where

import System.Environment (getArgs)
import Data.List.Split (splitOn)
import Text.Printf (printf)

type Point   = (Int, Int)
data Dir     = U Int | D Int | L Int | R Int deriving (Eq, Show)
data Orient  = H | V deriving (Eq, Show)
data Segment = Segment Orient Int Point deriving Show


addp :: Point -> Point -> Point
addp (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

sortc :: Point -> Point
sortc (c1, c2) = (min c1 c2, max c1 c2)

manh :: Point -> Point -> Int
manh (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

manh0 :: Point -> Int
manh0 = manh (0, 0)


parseLine :: String -> [Dir]
parseLine line = map parseDir $ splitOn "," line
    where
        parseDir :: String -> Dir
        parseDir (d:n) = let ni = (read n :: Int) in case d of
            'U' -> U ni
            'D' -> D ni
            'L' -> L ni
            'R' -> R ni
            _   -> error "Undefined direction!"


delta :: Dir -> Point -> Point
delta (U n) (x, y) = (x, y + n)
delta (D n) (x, y) = (x, y - n)
delta (L n) (x, y) = (x - n, y)
delta (R n) (x, y) = (x + n, y)


getSeg :: Point -> Point -> Segment
getSeg (x1, y1) (x2, y2)
    | (x1 == x2) = Segment V x1 (sortc (y1, y2))
    | (y1 == y2) = Segment H y1 (sortc (x1, x2))


makeSegments :: [Dir] -> [Segment]
makeSegments dirs = go dirs (0, 0)
    where
        go [] _ = []
        go (d:ds) p = let p' = delta d p in (getSeg p p') : (go ds p')


intersect :: Segment -> Segment -> Maybe Point
intersect (Segment   H _ _) (Segment   H _ _) = Nothing
intersect (Segment   V _ _) (Segment   V _ _) = Nothing
intersect l@(Segment V _ _) r@(Segment H _ _) = intersect r l
intersect (Segment H y (x1, x2)) (Segment V x (y1, y2)) =
    case (y1 <= y && y <= y2 && x1 <= x && x <= x2) of
        True  -> Just (x, y)
        False -> Nothing


intersections :: [(Segment, Segment)] -> [Point]
intersections [] = []
intersections ((s1,s2):ss) = case s1 `intersect` s2 of
    (Just p) -> p : intersections ss
    Nothing  -> intersections ss


main = do
    args <- getArgs
    content <- (take 2 . lines) <$> readFile (head args)
    let (segs1:segs2:_) = map (makeSegments . parseLine) content
    let m = minimum $ map manh0 $ filter (/= (0,0)) $ intersections [(s1, s2) | s1 <- segs1, s2 <- segs2]
    printf "Part 1: %d\n" m

