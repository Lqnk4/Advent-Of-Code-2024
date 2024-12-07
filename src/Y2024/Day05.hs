module Y2024.Day05 where

import qualified AoC (middle)
import Data.List (partition, sortBy)
import Data.Tuple (swap)

main :: IO ()
main = do
  raw <- lines <$> readFile "data/Y2024/Day05/input.txt"
  putStr "Part 1: " >> print (part1 raw)
  putStr "Part 2: " >> print (part2 raw)

parseOrderRule :: String -> (Int, Int)
parseOrderRule [] = (-1, -1)
parseOrderRule str = read $ ['('] ++ map (\x -> if x == '|' then ',' else x) str ++ [')']

parseUpdate :: String -> [Int]
parseUpdate str = read $ ['['] ++ str ++ [']']

orderRules :: [String] -> [(Int, Int)]
orderRules = takeWhile (/= (-1, -1)) . map parseOrderRule

updatesList :: [String] -> [[Int]]
updatesList = map parseUpdate . drop 1 . dropWhile (/= "")

-- ordered pairs prioritizing the elements with lowest index
pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x : xs) = map (x,) xs ++ pairs xs

part1 :: [String] -> Int
part1 raw = sum . map (head . AoC.middle) . filter isValid . updatesList $ raw
  where
    rules = orderRules raw
    isValid :: [Int] -> Bool
    isValid = not . any ((`elem` rules) . swap) . pairs

part2 :: [String] -> Int
part2 raw = sum . map (head . AoC.middle) $ fixed
  where
    rules = orderRules raw
    updates = updatesList raw
    isValid :: [Int] -> Bool
    isValid = not . any ((`elem` rules) . swap) . pairs
    (_, invalid) = partition isValid updates
    cmp :: Int -> Int -> Ordering
    cmp x y
      | (x, y) `elem` rules = LT
      | (y, x) `elem` rules = GT
    cmp _ _ = EQ
    fixed = map (sortBy cmp) invalid
