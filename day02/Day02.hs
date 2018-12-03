module Day02 where

import Data.List


data Box = Box { getId :: String } deriving (Eq)

parse :: String -> [Box]
parse = map Box . lines

count :: Eq a => [a] -> a -> Int
count s c = length . filter (==c) $ s

appearsExactlyKTimes :: Int -> String -> Bool
appearsExactlyKTimes k s = k `elem` (count s <$> nub s)

appearsExactlyTwoTimes :: Box -> Bool
appearsExactlyTwoTimes = appearsExactlyKTimes 2 . getId

appearsExactlyThreeTimes :: Box -> Bool
appearsExactlyThreeTimes = appearsExactlyKTimes 3 . getId

checksum :: [Box] -> Int
checksum boxes = exactlyTwo * exactlyThree
  where exactlyTwo   = count (appearsExactlyTwoTimes   <$> boxes) True
        exactlyThree = count (appearsExactlyThreeTimes <$> boxes) True

areCorrectBoxes :: Box -> Box -> Bool
areCorrectBoxes (Box x) (Box y) = differByOneChar x y

differByOneChar :: (Eq a) => [a] -> [a] -> Bool
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

commonLetters :: String -> String -> String
commonLetters (x:xs) (y:ys)
  | x /= y    =     commonLetters xs ys
  | otherwise = x : commonLetters xs ys
commonLetters _ _ = []

commonLettersOfCorrectBoxes :: [Box] -> String
commonLettersOfCorrectBoxes boxes = go $ uniquePairs boxes
  where
    go ((x, y):zs)
      | areCorrectBoxes x y = commonLetters (getId x) (getId y)
      | otherwise           = go zs

solvePartOne :: [Box] -> Int
solvePartOne = checksum

solvePartTwo :: [Box] -> String
solvePartTwo = commonLettersOfCorrectBoxes

main :: IO()
main = do
  contents <- readFile "input.txt"
  let boxes = parse contents
  putStrLn . show . solvePartOne $ boxes
  putStrLn .        solvePartTwo $ boxes


parseTest :: Bool
parseTest = [ Box "abcdef"
            , Box "bababc"
            , Box "aabcdd"] == parse "abcdef\nbababc\naabcdd\n"

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
