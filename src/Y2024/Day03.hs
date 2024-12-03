module Y2024.Day03 where

import Text.Regex.TDFA (AllTextMatches (getAllTextMatches), (=~))

main :: IO ()
main = do
    contents <- readFile "data/Y2024/Day03/input.txt"
    print $ part2 contents

applyRegex :: String -> [String]
applyRegex input = getAllTextMatches (input =~ "mul\\([0-9]+,[0-9]+\\)") :: [String]

part1 :: String -> Int
part1 = sum . map (uncurry (*) . read . filter (`notElem` ['a' .. 'z'])) . applyRegex

applyRegex2 :: String -> [String]
applyRegex2 input = getAllTextMatches (input =~ "mul\\([0-9]+,[0-9]+\\)|do\\(\\)|don't\\(\\)") :: [String]

part2 :: String -> Int
part2 = sum . map (uncurry (*) . read . filter (`notElem` ['a' .. 'z'])) . followsDo . applyRegex2

followsDo :: [String] -> [String]
followsDo [] = []
followsDo (x : xs)
    | x == "don't()" = followsDo . drop 1 . dropWhile (/= "do()") $ xs
    | otherwise = x : followsDo xs
