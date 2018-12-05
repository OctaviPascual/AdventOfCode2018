{-# OPTIONS_GHC -Wall #-}

module Day02 where

import Data.List


data Box = Box
  { boxId :: String }
  deriving (Show, Eq)

parseInput :: String -> [Box]
parseInput = fmap Box . lines

count :: Eq a => [a] -> a -> Int
count s c = length . filter (==c) $ s

appearsExactlyKTimes :: Int -> String -> Bool
appearsExactlyKTimes k s = k `elem` (count s <$> nub s)

appearsExactlyTwoTimes :: Box -> Bool
appearsExactlyTwoTimes = appearsExactlyKTimes 2 . boxId

appearsExactlyThreeTimes :: Box -> Bool
appearsExactlyThreeTimes = appearsExactlyKTimes 3 . boxId

checksum :: [Box] -> Int
checksum boxes = exactlyTwo * exactlyThree
  where exactlyTwo   = count (appearsExactlyTwoTimes   <$> boxes) True
        exactlyThree = count (appearsExactlyThreeTimes <$> boxes) True

areCorrectBoxes :: Box -> Box -> Bool
areCorrectBoxes (Box x) (Box y) = differByOneChar x y

differByOneChar :: Eq a => [a] -> [a] -> Bool
differByOneChar (x:xs) (y:ys)
  | x /= y    = xs == ys
  | otherwise = differByOneChar xs ys
differByOneChar _ _ = False

-- https://stackoverflow.com/a/28191224
uniquePairs :: [a] -> [(a, a)]
uniquePairs l = do
  (x:xs) <- tails l
  y      <- xs
  return (x, y)

commonLetters :: Eq a => [a] -> [a] -> [a]
commonLetters xs ys = foldr go [] $ zip xs ys
  where go (x, y) acc = if x == y then x : acc else acc

commonLettersOfCorrectBoxes :: [Box] -> String
commonLettersOfCorrectBoxes boxes = go . uniquePairs $ boxes
  where go ((x, y):zs)
          | areCorrectBoxes x y = commonLetters (boxId x) (boxId y)
          | otherwise           = go zs
        go [] = error "Empty list of boxes"

solvePartOne :: [Box] -> Int
solvePartOne = checksum

solvePartTwo :: [Box] -> String
solvePartTwo = commonLettersOfCorrectBoxes

main :: IO()
main = do
  contents <- readFile "input.txt"
  let boxes = parseInput contents
  putStrLn . show . solvePartOne $ boxes
  putStrLn .        solvePartTwo $ boxes


parseInputTest :: Bool
parseInputTest = [ Box "abcdef"
                 , Box "bababc"
                 , Box "aabcdd"] == parseInput "abcdef\nbababc\naabcdd\n"

solvePartOneTest :: Bool
solvePartOneTest = and
  [
    12 == solvePartOne [ Box "abcdef"
                       , Box "bababc"
                       , Box "abbcde"
                       , Box "abcccd"
                       , Box "aabcdd"
                       , Box "abcdee"
                       , Box "ababab"]
  ]

solvePartTwoTest :: Bool
solvePartTwoTest = and
  [
    "fgij" == solvePartTwo [ Box "abcde"
                           , Box "fghij"
                           , Box "klmno"
                           , Box "pqrst"
                           , Box "fguij"
                           , Box "axcye"
                           , Box "wvxyz"]
  ]
