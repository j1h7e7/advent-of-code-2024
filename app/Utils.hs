module Utils where

split :: Char -> String -> [String]
split splitChar inputString = case dropWhile (== splitChar) inputString of
  "" -> []
  s1 -> chunk : split splitChar leftoverChunks
    where
      (chunk, leftoverChunks) = break (== splitChar) s1

transpose :: [[a]] -> [[a]]
transpose ([] : _) = [] -- TODO: what does this line do?
transpose x = (map head x) : transpose (map tail x)

reduce :: (a -> a -> a) -> a -> [a] -> a
reduce p v [] = v
reduce predicate startval array = reduce predicate (predicate startval (head array)) (tail array)

bifurcate :: [a] -> (a, a)
bifurcate x = ((head x), (head (tail x)))
