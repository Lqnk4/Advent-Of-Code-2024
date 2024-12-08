-- | Day 8

module Y2024.Day08 where

import Data.List
import qualified Data.Map.Strict as Map

type Grid = [[Char]]

main :: IO ()
main = do
  raw <- lines <$> readFile "data/Y2024/Day08/input.txt"
  print $ part1 raw
  print $ part2 raw


coords :: Grid -> [(Int, Int)]
coords grid = [(x, y) | y <- [0..length grid - 1], x <- [0..(length . head $ grid) - 1]]

pairNodes :: (Eq a) => [a] -> [(a, a)]
pairNodes xs = [(x, y) | x <- xs, y <- xs, x /= y]

parseInput :: Grid -> Map.Map Char [(Int, Int)]
parseInput grid = foldr addNode Map.empty (coords grid)
  where
    addNode (x, y) m =
      let c = grid !! y !! x
      in if c == '.' then m else Map.insertWith (++) c [(x,y)] m

antiNode :: (Int, Int) -> (Int, Int) -> (Int, Int)
antiNode (x1, y1) (x2, y2) = (2 * x2 - x1, 2 * y2 - y1)

allAntiNodes :: Grid -> Map.Map Char [(Int, Int)] -> [(Int, Int)]
allAntiNodes grid m = filter (inGrid grid) $ nub $ concatMap (fmap (uncurry antiNode) . pairNodes) $ Map.elems m

inGrid :: Grid -> (Int, Int) -> Bool
inGrid grid (x, y) = x >= 0 && y >= 0 && x < (length . head $ grid) && y < (length $ grid)


part1 :: Grid -> Int
part1 grid = length . allAntiNodes grid . parseInput $ grid

antiNode' :: Grid -> ((Int, Int), (Int, Int)) -> [(Int, Int)]
antiNode' grid ((x1, y1), (x2, y2)) = takeWhile (inGrid grid) . map fst $ iterate (\((x1, y1), (dx, dy)) -> ((x1, y1) `add2` (dx, dy), (dx, dy))) ((x1, y1), (x2, y2) `sub2` (x1, y1))

allAntiNodes' :: Grid -> Map.Map Char [(Int, Int)] -> [(Int, Int)]
allAntiNodes' grid m = filter (inGrid grid) $ nub $ concatMap ( concatMap (antiNode' grid) . pairNodes) $ Map.elems m

part2 :: Grid -> Int
part2 grid = length . allAntiNodes' grid . parseInput $ grid

add2 :: (Int, Int) -> (Int, Int) -> (Int, Int)
add2 (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

sub2 :: (Int, Int) -> (Int, Int) -> (Int, Int)
sub2 (x2, y2) (x1, y1)  = (x2 - x1, y2 - y1)
