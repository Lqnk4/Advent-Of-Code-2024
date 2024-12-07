module Y2024.Day07 where

main :: IO ()
main = do
  raw <- lines <$> readFile "data/Y2024/Day07/input.txt"
  let tasks = parseInput raw
  print $ part1 tasks
  print $ part2 tasks

parseInput :: [String] -> [(Int, [Int])]
parseInput = map (\xs -> (target xs, nums xs))
  where
    target = read . takeWhile (/= ':')
    nums = map read . drop 1 . words

part1 :: [(Int, [Int])] -> Int
part1 tasks = sum $ map loop tasks
  where
    loop :: (Int, [Int]) -> Int
    loop (_, []) = 0
    loop (_, [x]) = x
    loop (target, x : y : xs)
      | target == loop (target, (x + y) : xs) = target
      | target == loop (target, (x * y) : xs) = target
      | otherwise = 0

part2 :: [(Int, [Int])] -> Int
part2 tasks = sum $ map loop tasks
  where
    loop :: (Int, [Int]) -> Int
    loop (_, []) = 0
    loop (_, [x]) = x
    loop (target, x : y : xs)
      | target == loop (target, (x + y) : xs) = target
      | target == loop (target, (x * y) : xs) = target
      | target == loop (target, read (show x ++ show y) : xs) = target
      | otherwise = 0
