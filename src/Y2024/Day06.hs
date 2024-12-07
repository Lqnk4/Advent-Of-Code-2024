module Y2024.Day06 where

import Data.List (elemIndex, findIndex)
import Data.Maybe (fromJust)
import Data.Set (Set, delete, empty, insert, member, toList)
data Block = Obstructions | Empty | Guard deriving (Show, Eq)
data Direction = U | D | L | R deriving (Show, Eq, Ord)
type Grid = [[Block]]
type BlockFn = ((Int, Int) -> Block, Int, Int)

main :: IO ()
main = do
  raw <- lines <$> readFile "data/Y2024/Day06/input.txt"
  let (blocks, origin) = parseInput raw
  let blocks' = map (\x -> if x == Guard then Empty else x) <$> blocks
  let visited = walk blocks' origin
  print $ length visited
  print $ length $ filter (hasLoop origin) $ addObstructions blocks' (toList $ delete origin visited)

turnRight :: Direction -> Direction
turnRight U = R
turnRight R = D
turnRight D = L
turnRight L = U

charToBlock :: Char -> Block
charToBlock '.' = Empty
charToBlock '#' = Obstructions
charToBlock '^' = Guard
charToBlock _ = Empty

parseInput :: [[Char]] -> (Grid, (Int, Int))
parseInput xs = (blocks, (x, y))
  where
    blocks = map charToBlock <$> xs
    x = fromJust (findIndex (Guard `elem`) blocks)
    y = fromJust $ elemIndex Guard $ blocks !! x

walk :: Grid -> (Int, Int) -> Set (Int, Int)
walk blocks (x, y) = loop U (x, y) empty
  where
    outOfBounds blocks (x, y) = x < 0 || x >= length blocks || y < 0 || y >= length (head blocks)
    isEmpty blocks (x, y) = blocks !! x !! y == Empty
    loop dir (x, y) n
      | outOfBounds blocks next = insert (x, y) n
      | isEmpty blocks next = loop dir next (insert (x, y) n)
      | otherwise = loop (turnRight dir) (x, y) n
      where next = nextPos dir (x, y)

hasLoop :: (Int, Int) -> BlockFn -> Bool
hasLoop o (bf, xx, yy) = loop U o empty
  where
    outOfBounds (x, y) = x < 0 || x >= xx || y < 0 || y >= yy
    isEmpty (x, y) = bf (x, y) == Empty
    loop dir (x, y) states
      | outOfBounds next = False
      | isRecurrent (x, y) dir states = True
      | isEmpty next = loop dir next (insert ((x, y), dir) states)
      | otherwise = loop (turnRight dir) (x, y) states
      where
        next = nextPos dir (x, y)
        isRecurrent (x, y) dir states = ((x, y), dir) `member` states

addObstructions :: Grid -> [(Int, Int)] -> [((Int, Int) -> Block, Int, Int)]
addObstructions blocks = map makeBlockFn
  where
    w = length (head blocks)
    h = length blocks
    makeBlockFn p1 = (\p2@(x, y) -> if p1 == p2 then Obstructions else blocks !! x !! y, h, w)

nextPos :: Direction -> (Int, Int) -> (Int, Int)
nextPos U (x, y) = (x - 1, y)
nextPos D (x, y) = (x + 1, y)
nextPos L (x, y) = (x, y - 1)
nextPos R (x, y) = (x, y + 1)
