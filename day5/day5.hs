module Main where

import Control.Applicative (Alternative (empty, many, (<|>)))
import Control.Monad ((>=>))
import Data.Char (isDigit)
import qualified Data.Map as M
import Data.Maybe (isNothing)
import Utils.Parser
  ( Parser (runParser),
    charP,
    intParser,
    sepByParser,
    wsParser,
  )

input :: Int
input = 5

commaSeparatedIntP :: Parser [Int]
commaSeparatedIntP = sepByParser (wsParser *> charP ',' <* wsParser) intParser

parseInput :: [Char] -> [Int]
parseInput xs = let Just (vals, _) = runParser commaSeparatedIntP xs in vals

padInstruction :: Int -> [Int]
padInstruction x = let xs = show x in map (\x -> read [x]) $ until ((==) 4 . length) ('0' :) xs

compute :: Int -> [Int] -> M.Map Int Int -> (Int, [Int], M.Map Int Int)
compute pointer directions indexedInstructions = case directions of
  [_, _, 9, 9] -> (pointer, [], M.empty)
  [_, _, _, 3] ->
    let location = M.lookup (pointer + 1) indexedInstructions
     in execute location (Just input) (pointer + 2) []
  [_, _, _, 4] ->
    let Just location = M.lookup (pointer + 1) indexedInstructions
        Just val = M.lookup location indexedInstructions
     in execute Nothing Nothing (pointer + 2) [val]
  [b, c, _, e] ->
    let Just param1 = M.lookup (pointer + 1) indexedInstructions
        Just param2 = M.lookup (pointer + 2) indexedInstructions
        p1 = getVal c param1
        p2 = getVal b param2
     in if e `elem` [5, 6]
          then
            let Just pointer' = getIP e p1 p2 pointer
             in execute Nothing Nothing pointer' []
          else
            let location = M.lookup (pointer + 3) indexedInstructions
                val = getValueToStore e p1 p2
             in execute location val (pointer + 4) []
  where
    getVal mode p
      | mode == 0 = let Just val = M.lookup p indexedInstructions in val
      | mode == 1 = p

    getValueToStore 1 p1 p2 = Just (p1 + p2)
    getValueToStore 2 p1 p2 = Just (p1 * p2)
    getValueToStore 7 p1 p2 = if p1 < p2 then Just 1 else Just 0
    getValueToStore 8 p1 p2 = if p1 == p2 then Just 1 else Just 0

    getIP opCode a b ip
      | opCode == 5 = Just $ if a /= 0 then b else ip + 3
      | opCode == 6 = Just $ if a == 0 then b else ip + 3
      | otherwise = Nothing

    execute :: Maybe Int -> Maybe Int -> Int -> [Int] -> (Int, [Int], M.Map Int Int)
    execute location val pointer outputs
      | isNothing location || isNothing val = (pointer, outputs, indexedInstructions)
      | otherwise = let Just l = location; Just v = val in (pointer, outputs, M.insert l v indexedInstructions)

runInstructions :: Int -> M.Map Int Int -> [Int]
runInstructions pointer indexedInstructions =
  if null nextState || pointer >= length indexedInstructions
    then outputs
    else runInstructions nextPointer nextState ++ outputs
  where
    parameters = let Just val = M.lookup pointer indexedInstructions in val
    directions = padInstruction parameters
    computation = compute pointer directions indexedInstructions
    nextPointer = let (val, _, _) = computation in val
    outputs = let (_, val, _) = computation in val
    nextState = let (_, _, val) = computation in val

main :: IO ()
main = do
  putStrLn "Enter File Name"
  fileName <- getLine
  fileContents <- readFile fileName
  let parsedInput = parseInput fileContents
  let indexedInstructions = zip [0 ..] parsedInput
  print $ runInstructions 0 (M.fromList indexedInstructions)