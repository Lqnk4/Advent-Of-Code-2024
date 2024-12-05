module Y2024.Day04 where

import qualified AoC
import Data.List

main :: IO ()
main = do
    contents <- readFile "data/Y2024/Day04/input.txt"
    print $ part1 contents
    print $ part2 contents


countXMAS :: String -> Int
countXMAS = length . filter ((||) <$> isPrefixOf "XMAS" <*> isPrefixOf "SAMX") . tails

part1 :: [Char] -> Int
part1 input = countForward grid + countDown grid + countDiagonals grid
  where
    grid = lines input


part2 :: String -> Int
part2 str =
  let grid = lines str
      groups = concat $ AoC.subArrays 3 $ transpose $ AoC.subArrays 3 grid
   in length $ filter checkX groups
  where
    checkX :: [[Char]] -> Bool
    checkX
      [ [a, _, b],
        [_, 'A', _],
        [c, _, d]
        ] = elem [a, d] ["MS", "SM"] && elem [b, c] ["MS", "SM"]
    checkX _ = False

countForward :: [String] -> Int
countForward = sum . map countXMAS

countDown :: [String] -> Int
countDown = sum . map countXMAS . AoC.rotate90

allDiagonals :: [[a]] -> [[a]]
allDiagonals xss = AoC.diagonals xss ++ AoC.antiDiagonals xss

countDiagonals :: [String] -> Int
countDiagonals = sum . map countXMAS . allDiagonals
