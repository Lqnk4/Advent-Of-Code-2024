module AoC where

import Data.List (tails, transpose)

-- rotate a list matrix degrees CCW
rotate90 :: [[a]] -> [[a]]
rotate90 = reverse . transpose

-- rotate a matrix 180 degrees
rotate180 :: [[a]] -> [[a]]
rotate180 = rotate90 . rotate90

-- extract diagonals from a matrix
diagonals :: [[a]] -> [[a]]
diagonals =
    (++)
        <$> reverse . transpose . zipWith drop [0 ..]
        <*> transpose . zipWith drop [1 ..] . transpose

-- extract antidiagonals from a matrix
antiDiagonals :: [[a]] -> [[a]]
antiDiagonals = diagonals . rotate90

-- get all subarrays of size n
subArrays :: Int -> [[a]] -> [[[a]]]
subArrays n xss = [[take n t | t <- tails xs] | xs <- xss]
