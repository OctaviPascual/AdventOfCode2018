{-# OPTIONS_GHC -Wall #-}

module Day03 where

import           Data.List
import           Text.ParserCombinators.ReadP
import qualified Data.Map.Strict as Map

data Claim = Claim
  { claimId       :: Int
  , topLeftCorner :: Point
  , width         :: Int
  , height        :: Int }
  deriving (Show, Eq)

data Point = Point Int Int deriving (Show, Eq, Ord)

type Fabric = Map.Map Point [Claim]

parseInt :: ReadP Int
parseInt = read <$> munch (`elem` ['0'..'9'])

parseId :: ReadP Int
parseId = char '#' *> parseInt

parseLeftEdge :: ReadP Int
parseLeftEdge = skipSpaces *> char '@' *> skipSpaces *> parseInt

parseTopEdge :: ReadP Int
parseTopEdge = char ',' *> parseInt

parseWidth :: ReadP Int
parseWidth = char ':' *> skipSpaces *> parseInt

parseHeight :: ReadP Int
parseHeight = char 'x' *> parseInt

parseLine :: ReadS Claim
parseLine =
  readP_to_S
  $ pure Claim
  <*> parseId
  <*> (pure Point <*> parseLeftEdge <*> parseTopEdge)
  <*> parseWidth
  <*> parseHeight
  <* eof

parseInput :: String -> [Claim]
parseInput = concatMap (fmap fst) . fmap parseLine . lines

claimedPoints :: Claim -> [Point]
claimedPoints (Claim _ (Point i j) w h) = do
  x <- [i..i+w-1]
  y <- [j..j+h-1]
  return $ Point x y

claimOne :: Fabric -> Claim -> Fabric
claimOne fabric claim = foldl' go fabric $ claimedPoints claim
  where go fabric' point = Map.insertWith (++) point [claim] fabric'

claimAll :: [Claim] -> Fabric
claimAll = foldl' claimOne Map.empty

solvePartOne :: [Claim] -> Int
solvePartOne = length . Map.filterWithKey (\_ v -> length v >= 2) . claimAll

solvePartTwo :: [Claim] -> Int
solvePartTwo cs = claimId $ head $ cs \\ go cs 
  where go = concat . Map.elems . Map.filterWithKey (\_ v -> length v >= 2) . claimAll

main :: IO()
main = do
  contents <- readFile "input.txt"
  let x = parseInput contents
  putStrLn . show . solvePartOne $ x
  putStrLn . show . solvePartTwo $ x


parseInputTest :: Bool
parseInputTest = [ Claim 1 (Point 21 44) 55 61
                 , Claim 2 (Point 25 42) 97 23] == parseInput input
  where input = "#1 @ 21,44: 55x61\n#2 @ 25,42: 97x23\n"

solvePartOneTest :: Bool
solvePartOneTest = 4 == solvePartOne [ Claim 1 (Point 1 3) 4 4
                                     , Claim 2 (Point 3 1) 4 4
                                     , Claim 3 (Point 5 5) 2 2]

solvePartTwoTest :: Bool
solvePartTwoTest = 3 == solvePartTwo [ Claim 1 (Point 1 3) 4 4
                                     , Claim 2 (Point 3 1) 4 4
                                     , Claim 3 (Point 5 5) 2 2]
