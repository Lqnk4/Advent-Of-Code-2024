module Y2024.Day01 where
import Data.List


main :: IO ()
main = do
    handle <- readFile "data/Y2024/Day01/input.txt"
    let parsedLists = uninterleave . parseNums $ handle
    print $ uncurry listDiffSum parsedLists
    print $ uncurry similarityScore parsedLists


parseNums :: String -> [Int]
parseNums = map read . concatMap words . lines


listDiffSum :: (Num a, Ord a) => [a] -> [a] -> a
listDiffSum xs ys = sum $ zipWith ((abs .) . (-)) (sort xs) (sort ys)

similarityScore :: (Num a, Eq a) => [a]  -> [a] -> a
similarityScore xs ys = sum $ [x * genericLength (filter (==x) ys) | x <- xs]


-- | split a list into two lists in an alternating way
--
-- @uninterleave [1,2,3,4,5,6] == ([1,3,5],[2,4,6])@
--
-- It's a special case of 'Numeric.Random.Spectrum.Pink.split'.

uninterleave :: [a] -> ([a],[a])
uninterleave = foldr (\x ~(xs,ys) -> (x:ys,xs)) ([],[])


