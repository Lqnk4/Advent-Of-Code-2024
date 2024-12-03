module Y2024.Day02 where

import Data.List

main :: IO ()
main = do
    contents <- readFile "data/Y2024/Day02/input.txt"
    let parsed = parseReports contents
    print $ numSafe parsed
    print $ numSafeDampened parsed

parseReports :: String -> [[Int]]
parseReports = map (map read . words) . lines

isSafe :: (Num a, Ord a) => [a] -> Bool
isSafe xs = isDecreasingByNum xs || isIncreasingByNum xs

isSafeDampened :: (Num a, Ord a) => [a] -> Bool
isSafeDampened xs = isSafe xs || any isSafe (combinations (length xs - 1) xs)

isIncreasingByNum :: (Num a, Ord a) => [a] -> Bool
isIncreasingByNum [] = True
isIncreasingByNum [_] = True
isIncreasingByNum (x : y : xs) = x < y && abs (x - y) <= 3 && isIncreasingByNum (y : xs)

isDecreasingByNum :: (Num a, Ord a) => [a] -> Bool
isDecreasingByNum [] = True
isDecreasingByNum [_] = True
isDecreasingByNum (x : y : xs) = x > y && abs (x - y) <= 3 && isDecreasingByNum (y : xs)

numSafe :: [[Int]] -> Int
numSafe = length . filter isSafe

numSafeDampened :: [[Int]] -> Int
numSafeDampened = length . filter isSafeDampened


safePredicate :: Ordering -> Int -> Int -> Bool
safePredicate GT x y = x > y && abs (x-y) <= 3
safePredicate LT x y = x < y && abs (x-y) <= 3
safePredicate EQ _ _ = False

combinations :: Int -> [a] -> [[a]]
combinations k = filter ((k==).length) . subsequences
