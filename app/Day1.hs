module Day1 where

import Data.List (sort)
import Utils

getColumns :: String -> [[Integer]]
getColumns inputString =
  map (map read) strColumns
  where
    strColumns = map (split ' ') (split '\n' inputString)

getDiff :: [Integer] -> Integer
getDiff [] = 0
getDiff a = abs ((head a) - (head (tail a)))

solve :: String -> Integer
solve inputString =
  reduce (\a b -> a + b) 0 diffs
  where
    columns = transpose (getColumns inputString)
    sortedCols = map sort columns
    diffs = map getDiff (transpose sortedCols)

simScore :: [Integer] -> Integer -> Integer
simScore array target =
  target * count
  where
    count = toInteger (length (filter (== target) array))

solve2 :: String -> Integer
solve2 inputString =
  reduce (\a b -> a + b) 0 scores
  where
    columns = transpose (getColumns inputString)
    (leftList, rightList) = bifurcate columns
    scores = map (simScore rightList) leftList
