module Day2 where

import Utils

pairwiseObeys :: [Integer] -> (Integer -> Integer -> Bool) -> Bool
pairwiseObeys [] c = True
pairwiseObeys [_] c = True
pairwiseObeys sequence condition =
  c1 && c2
  where
    c1 = condition (head sequence) (head (tail sequence))
    c2 = pairwiseObeys (tail sequence) condition

isSafe :: [Integer] -> Bool
isSafe report =
  pairwiseObeys report safeIncreasing || pairwiseObeys report safeDecreasing
  where
    safeIncreasing = \a b -> b > a && (b - a) >= 1 && (b - a) <= 3
    safeDecreasing = \b a -> b > a && (b - a) >= 1 && (b - a) <= 3

solve :: String -> Integer
solve inputString =
  toInteger (length (filter isSafe reports))
  where
    reports = map (map read) strNumbers
    strNumbers = map (split ' ') (split '\n' inputString)

singleRemovals :: [a] -> [[a]]
singleRemovals [] = [[]]
singleRemovals [_] = [[]]
singleRemovals x = singleRemovalIDX x ((toInteger (length x)) - 1)

singleRemovalIDX :: [a] -> Integer -> [[a]]
singleRemovalIDX _ (-1) = []
singleRemovalIDX list idx =
  newList : (singleRemovalIDX list (idx - 1))
  where
    newList = remove idx list

remove :: Integer -> [a] -> [a]
remove _ [] = []
remove 0 (x : xs) = xs
remove n (x : xs) = x : remove (n - 1) (xs)

isSafeV2 :: [Integer] -> Bool
isSafeV2 report =
  any (== True) (map isSafe dampenedReports)
  where
    dampenedReports = singleRemovals report

solve2 :: String -> Integer
solve2 inputString =
  toInteger (length (filter isSafeV2 reports))
  where
    reports = map (map read) strNumbers
    strNumbers = map (split ' ') (split '\n' inputString)
