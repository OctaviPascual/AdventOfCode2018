module Day01 where

import qualified Data.Set as S

numberToInt :: String -> Int
numberToInt ('+':n) = read n
numberToInt n       = read n

parse :: String -> [Int]
parse = map numberToInt . lines

firstDuplicate :: [Int] -> Int
firstDuplicate = go S.empty
  where
    go seen (x:xs)
      | x `S.member` seen = x
      | otherwise         = go (x `S.insert` seen) xs

solvePartOne :: [Int] -> Int
solvePartOne = sum

solvePartTwo :: [Int] -> Int
solvePartTwo = firstDuplicate . scanl (+) 0 . cycle

main :: IO()
main = do
  contents <- readFile "input.txt"
  let frequencyChanges = parse contents
  putStrLn . show . solvePartOne $ frequencyChanges
  putStrLn . show . solvePartTwo $ frequencyChanges


parseTest :: Bool
parseTest = [1, -2, 3, -4] == parse "+1\n-2\n+3\n-4\n"

solvePartOneTest :: Bool
solvePartOneTest = and
  [
    3  == solvePartOne [1, 1, 1],
    0  == solvePartOne [1, 1, -2],
    -6 == solvePartOne [-1, -2, -3]
  ]

solvePartTwoTest :: Bool
solvePartTwoTest = and
  [
    0  == solvePartTwo [1, -1],
    10 == solvePartTwo [3, 3, 4, -2, -4],
    5  == solvePartTwo [-6, 3, 8, 5, -6],
    14 == solvePartTwo [7, 7, -2, -7, -4]
  ]
