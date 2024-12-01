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
