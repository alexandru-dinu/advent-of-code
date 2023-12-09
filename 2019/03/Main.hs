module Main where

import Data.List.Split (splitOn)
import System.Environment (getArgs)
import Text.Printf (printf)

type Point = (Int, Int)

data Dir
    = U Int
    | D Int
    | L Int
    | R Int
    deriving (Eq, Show)

data Orient
    = H
    | V
    deriving (Eq, Show)

data Segment =
    Segment Orient Int Point
    deriving (Show)

addp :: Point -> Point -> Point
addp (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

manh :: Point -> Point -> Int
manh (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

parseLine :: String -> [Dir]
parseLine line = map parseDir $ splitOn "," line
  where
    parseDir :: String -> Dir
    parseDir (d:n) =
        let ni = (read n :: Int)
         in case d of
                'U' -> U ni
                'D' -> D ni
                'L' -> L ni
                'R' -> R ni
                _ -> error "Undefined direction!"

delta :: Dir -> Point -> Point
delta (U n) (x, y) = (x, y + n)
delta (D n) (x, y) = (x, y - n)
delta (L n) (x, y) = (x - n, y)
delta (R n) (x, y) = (x + n, y)

getSeg :: Point -> Point -> Segment
getSeg (x1, y1) (x2, y2)
    | x1 == x2 = Segment V x1 (y1, y2)
    | y1 == y2 = Segment H y1 (x1, x2)

makeSegments :: [Dir] -> [Segment]
makeSegments dirs = go dirs (0, 0)
  where
    go [] _ = []
    go (d:ds) p =
        let p' = delta d p
         in getSeg p p' : go ds p'

bw :: Int -> Int -> Int -> Bool
bw a b x = min a b <= x && x <= max a b

intersect :: Segment -> Segment -> Maybe Point
intersect (Segment H _ _) (Segment H _ _) = Nothing
intersect (Segment V _ _) (Segment V _ _) = Nothing
intersect l@(Segment V _ _) r@(Segment H _ _) = intersect r l
intersect (Segment H y (x1, x2)) (Segment V x (y1, y2)) =
    if bw x1 x2 x && bw y1 y2 y
        then Just (x, y)
        else Nothing

intersections :: [(Segment, Segment)] -> [Point]
intersections [] = []
intersections ((s1, s2):ss) =
    case s1 `intersect` s2 of
        (Just p) -> p : intersections ss
        Nothing -> intersections ss

distToIx :: Point -> [Segment] -> Int
distToIx ix (seg:segs) =
    case go ix seg of
        (True, d) -> d
        (False, d) -> d + distToIx ix segs
  where
    go :: Point -> Segment -> (Bool, Int)
    go (_x, _y) (Segment H y (x1, x2)) =
        if _y /= y
            then (False, abs (x1 - x2))
            else (True, abs (x1 - _x))
    go (_x, _y) (Segment V x (y1, y2)) =
        if _x /= x
            then (False, abs (y1 - y2))
            else (True, abs (y1 - _y))

main = do
    args <- getArgs
    content <- take 2 . lines <$> readFile (head args)
    let (segs1:segs2:_) = map (makeSegments . parseLine) content
    let ixs = filter (/= (0, 0)) $ intersections [(s1, s2) | s1 <- segs1, s2 <- segs2]
    printf "Part 1: %d\n" $ minimum $ map (manh (0, 0)) ixs
    printf "Part 2: %d\n" $ minimum [distToIx ix segs1 + distToIx ix segs2 | ix <- ixs]
