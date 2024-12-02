import Data.List

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let parsed = parseReports contents
    print $ numSafe parsed
    print $ numSafeDampened parsed

parseReports :: String -> [[Int]]
parseReports contents = map (map read . words) (lines contents)

isSafe :: (Num a, Ord a) => [a] -> Bool
isSafe [] = True
isSafe [_] = True
isSafe xs = isDecreasingByNum xs || isIncreasingByNum xs

isSafeDampened :: (Num a, Ord a) => [a] -> Bool
isSafeDampened [] = True
isSafeDampened [_] = True
isSafeDampened xs = isSafe xs || any isSafe (combinations (length xs - 1) xs)

isIncreasingByNum :: (Num a, Ord a) => [a] -> Bool
isIncreasingByNum [] = True
isIncreasingByNum [_] = True
isIncreasingByNum all@(x : y : xs) = x < y && abs (x - y) <= 3 && isIncreasingByNum (y : xs)

isDecreasingByNum :: (Num a, Ord a) => [a] -> Bool
isDecreasingByNum [] = True
isDecreasingByNum [_] = True
isDecreasingByNum all@(x : y : xs) = x > y && abs (x - y) <= 3 && isDecreasingByNum (y : xs)

numSafe :: [[Int]] -> Int
numSafe = length . filter isSafe

numSafeDebug :: [[Int]] -> [Bool]
numSafeDebug = map isSafe

numSafeDampened :: [[Int]] -> Int
numSafeDampened = length . filter isSafeDampened


safePredicate :: Ordering -> Int -> Int -> Bool
safePredicate GT x y = x > y && abs (x-y) <= 3
safePredicate LT x y = x < y && abs (x-y) <= 3

combinations :: Int -> [a] -> [[a]]
combinations k ns = filter ((k==).length) $ subsequences ns
