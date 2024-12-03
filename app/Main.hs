module Main where

import qualified Day1
import qualified Day2

solve :: String -> String -> (String -> Integer)
solve "1" "1" = Day1.solve
solve "1" "2" = Day1.solve2
solve "2" "1" = Day2.solve

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
  putStrLn "Enter part number:"
  part <- getLine
  putStrLn "Run test?"
  test <- getLine
  putStrLn "Answer is:"
  contents <- readFile (getFileName day (parseBool test))
  print $ solve day part contents
