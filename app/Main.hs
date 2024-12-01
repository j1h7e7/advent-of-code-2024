module Main where

import qualified Day1

solve :: String -> (String -> Integer)
solve "1" = Day1.solve

getFileName :: String -> Bool -> String
getFileName day isTest = "../input/day" ++ day ++ testText ++ ".txt"
  where
    testText = if isTest then "test" else ""

parseBool :: String -> Bool
parseBool "y" = True
parseBool "n" = False
parseBool "" = False

main :: IO ()
main = do
  putStrLn "Enter day number:"
  day <- getLine
  putStrLn "Run test?"
  test <- getLine
  putStrLn "Answer is:"
  contents <- readFile (getFileName day (parseBool test))
  print $ solve day contents
