module Day2 where

import Utils

pairwiseObeys :: [Integer] -> (Integer -> Integer -> Bool) -> Bool
pairwiseObeys [] c = True
pairwiseObeys [x] c = True
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