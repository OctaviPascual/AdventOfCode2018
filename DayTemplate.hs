module DayX where

parse :: String -> a
parse = undefined

solvePartOne :: a -> Int
solvePartOne = undefined

solvePartTwo :: a -> Int
solvePartTwo = undefined

main :: IO()
main = do
  contents <- readFile "input.txt"
  let x = parse contents
  putStrLn . show . solvePartOne $ x
  putStrLn . show . solvePartTwo $ x


parseTest :: Bool
parseTest = [] == parse ""

solvePartOneTest :: Bool
solvePartOneTest = and
  [
  ]

solvePartTwoTest :: Bool
solvePartTwoTest = and
  [
  ]
