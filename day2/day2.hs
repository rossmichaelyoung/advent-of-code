module Main where

import Control.Applicative (Alternative (empty, many, (<|>)))
import Control.Monad ((>=>))
import Data.Char (isDigit)
import Utils.Parser

commaSeparatedIntP :: Parser [Int]
commaSeparatedIntP = sepByParser (wsParser *> charP ',' <* wsParser) naturalNumParser

parseInput :: [Char] -> [Int]
parseInput xs = let Just (vals, _) = runParser commaSeparatedIntP xs in vals

alterInput :: Int -> Int -> [Int] -> [Int]
alterInput noun verb = map (snd . (\(index, val) -> if index == 1 then (index, noun) else (if index == 2 then (index, verb) else (index, val)))) . zip [0 ..]

compute :: Int -> Int -> Int -> Int -> [Int] -> [Int]
compute operation val1 val2 store xs
  | operation == 1 = let newVal = (xs !! val1) + (xs !! val2) in map (\(val, index) -> if index == store then newVal else val) indexedList
  | operation == 2 = let newVal = (xs !! val1) * (xs !! val2) in map (\(val, index) -> if index == store then newVal else val) indexedList
  | otherwise = []
  where
    indexedList = zip xs [0 ..]

group :: Int -> [a] -> [[a]]
group n [] = []
group n xs = take n xs : group n (drop n xs)

runInstructions :: Int -> [Int] -> [[Int]] -> [Int]
runInstructions groupingIndex xs gs
  | length currentGrouping < 4 || null newList = xs
  | otherwise = runInstructions (groupingIndex + 1) newList gs
  where
    currentGrouping = gs !! groupingIndex
    operation = head currentGrouping
    val1 = currentGrouping !! 1
    val2 = currentGrouping !! 2
    store = last currentGrouping
    newList = compute operation val1 val2 store xs

solution :: [Int] -> Int -> [(Int, Int)] -> (Int, Int)
solution xs endingVal ys@((noun, verb) : cartesianNumbers)
  | null ys = (-1, -1)
  | currentEndingVal == endingVal = (noun, verb)
  | otherwise = solution xs endingVal cartesianNumbers
  where
    currentEndingVal = let newInstructions = alterInput noun verb xs in head $ runInstructions 0 newInstructions (groupedList newInstructions)
    groupedList = group 4

main :: IO Int
main = do
  putStrLn "Enter File Name"
  fileName <- getLine
  putStrLn "Enter Ending Value"
  endingVal <- getLine
  fileContents <- readFile fileName
  let (noun, verb) = solution (parseInput fileContents) (read endingVal) ((,) <$> [0 .. 99] <*> [0 .. 99]) in return (100 * noun + verb)
