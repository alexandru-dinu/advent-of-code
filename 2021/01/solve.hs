import System.Environment (getArgs)
import Text.Printf (printf)

slice :: Int -> Int -> [a] -> [a]
slice i m = take m . drop i

diff :: (Num a) => [a] -> [a]
diff xs = zipWith (-) (tail xs) xs

convolve :: (Num a) => [a] -> [a] -> [a]
convolve xs kern = [sum $ zipWith (*) (slice i m xs) kern | i <- [0..n - m]]
    where
        m = length kern
        n = length xs

part1 :: [Int] -> Int
part1 xs = sum $ map (\x -> if x > 0 then 1 else 0) $ diff xs

part2 :: [Int] -> Int
part2 xs = part1 $ convolve xs [1, 1, 1]

main :: IO ()
main = do
    args <- getArgs
    content <- readFile $ head args
    let xs = map read $ lines content
    printf "Part 1: %d\n" $ part1 xs
    printf "Part 2: %d\n" $ part2 xs