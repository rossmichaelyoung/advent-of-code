module Main where

import Control.Applicative (Alternative ((<|>)))
import Data.List
import qualified Data.Set as S
import Data.Tuple
import Utils.Parser

-- need to find all points wires cross and then find closest intersection to central port
-- each line is the directions from central port of each wire
-- central port at (0, 0)
-- R means add to x
-- L means subtract from x
-- U means add to y
-- D means subtract from y

-- parse input file using char parser to make a tuple of direction and magnitude

-- function takes a list of instructions for cable1, a list for cable2, the current position for cable1,
-- the current position for cable2, and a list of points where they overlap
-- if cable1 and cable2 are at the same point, add that point to the list where they overlap
-- when both lists are empty and have checked if the cables overlap here, then map manhattan distance from (0, 0) to
-- all points in the list of overlapping points and return the smallest manhattan distance

directionParser :: Char -> Parser (Char, Int)
directionParser x = (,) <$> charP x <*> intParser

rightParser :: Parser (Char, Int)
rightParser = directionParser 'R'

leftParser :: Parser (Char, Int)
leftParser = directionParser 'L'

upParser :: Parser (Char, Int)
upParser = directionParser 'U'

downParser :: Parser (Char, Int)
downParser = directionParser 'D'

directionsParser :: Parser (Char, Int)
directionsParser = rightParser <|> leftParser <|> upParser <|> downParser

commaSeparatedDirectionParser :: Parser [(Char, Int)]
commaSeparatedDirectionParser = sepByParser (wsParser *> charP ',' <* wsParser) directionsParser

parseInput :: [[Char]] -> ([(Char, Int)], [(Char, Int)])
parseInput xs =
  let Just (directions1, _) = runParser commaSeparatedDirectionParser (head xs)
      Just (directions2, _) = runParser commaSeparatedDirectionParser (last xs)
   in (directions1, directions2)

manhattanDistance :: (Int, Int) -> (Int, Int) -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

calculateNewPos :: (Int, Int) -> (Char, Int) -> [(Int, Int)]
calculateNewPos (x, y) (direction, magnitude)
  | direction == 'R' && magnitude > 0 = (x + 1, y) : calculateNewPos (x + 1, y) (direction, magnitude - 1)
  | direction == 'L' && magnitude > 0 = (x - 1, y) : calculateNewPos (x - 1, y) (direction, magnitude - 1)
  | direction == 'U' && magnitude > 0 = (x, y + 1) : calculateNewPos (x, y + 1) (direction, magnitude - 1)
  | direction == 'D' && magnitude > 0 = (x, y - 1) : calculateNewPos (x, y - 1) (direction, magnitude - 1)
  | otherwise = []

getPoints :: [(Char, Int)] -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
getPoints cableDirections cablePos cablePoints
  | null cableDirections = cablePoints
  | otherwise = getPoints remainingCableDirections newCablePos newCablePoints
  where
    newCablePositions = calculateNewPos cablePos (head cableDirections)
    newCablePos = if not (null newCablePositions) then last newCablePositions else cablePos
    newCablePoints = newCablePositions ++ cablePoints
    remainingCableDirections = tail cableDirections

calculateNewPosAndSteps :: ((Int, Int), Int) -> (Char, Int) -> [((Int, Int), Int)]
calculateNewPosAndSteps ((x, y), steps) (direction, magnitude)
  | direction == 'R' && magnitude > 0 = ((x + 1, y), steps + 1) : calculateNewPosAndSteps ((x + 1, y), steps + 1) (direction, magnitude - 1)
  | direction == 'L' && magnitude > 0 = ((x - 1, y), steps + 1) : calculateNewPosAndSteps ((x - 1, y), steps + 1) (direction, magnitude - 1)
  | direction == 'U' && magnitude > 0 = ((x, y + 1), steps + 1) : calculateNewPosAndSteps ((x, y + 1), steps + 1) (direction, magnitude - 1)
  | direction == 'D' && magnitude > 0 = ((x, y - 1), steps + 1) : calculateNewPosAndSteps ((x, y - 1), steps + 1) (direction, magnitude - 1)
  | otherwise = []

getPointsAndSteps :: [(Char, Int)] -> ((Int, Int), Int) -> [((Int, Int), Int)] -> [((Int, Int), Int)]
getPointsAndSteps cableDirections cablePosAndSteps cablePointsAndSteps
  | null cableDirections = cablePointsAndSteps
  | otherwise = getPointsAndSteps remainingCableDirections newCablePosAndSteps newCablePointsAndSteps
  where
    newCablePositionsAndSteps = calculateNewPosAndSteps cablePosAndSteps (head cableDirections)
    newCablePosAndSteps = if not (null newCablePositionsAndSteps) then last newCablePositionsAndSteps else cablePosAndSteps
    newCablePointsAndSteps = newCablePositionsAndSteps ++ cablePointsAndSteps
    remainingCableDirections = tail cableDirections

main = do
  putStrLn "Enter file name"
  fileName <- getLine
  fileContents <- readFile fileName
  let (cable1Directions, cable2Directions) = parseInput $ lines fileContents
  let cable1PointsAndSteps = map swap . sort . map swap $ getPointsAndSteps cable1Directions ((0, 0), 0) []
  let cable1Points = map fst cable1PointsAndSteps
  let cable2PointsAndSteps = map swap . sort . map swap $ getPointsAndSteps cable2Directions ((0, 0), 0) []
  let cable2Points = map fst cable2PointsAndSteps
  let intersections = S.toList $ S.intersection (S.fromList cable1Points) (S.fromList cable2Points)
  let maybeMinSteps = map (\point -> (+) <$> lookup point cable1PointsAndSteps <*> lookup point cable2PointsAndSteps) intersections
  let minSteps = minimum $ map (\(Just steps) -> steps) maybeMinSteps
  return minSteps