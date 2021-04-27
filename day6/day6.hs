{-# LANGUAGE TupleSections #-}

module Main where

import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Utils.Parser

newtype Orbit a = Orbit {orbit :: (a, a)} deriving (Eq, Show)

planet :: Orbit a -> a
planet = fst . orbit

satellite :: Orbit a -> a
satellite = snd . orbit

orbitParser :: Parser (Orbit String)
orbitParser = Orbit <$> ((,) <$> alphaNumStringParser <*> (charP ')' *> alphaNumStringParser))

newLineSepOrbitParser :: Parser [Orbit String]
newLineSepOrbitParser = sepByParser wsParser orbitParser

removeDuplicatesInOrbit :: [(String, String)] -> [(String, [String])]
removeDuplicatesInOrbit [] = []
removeDuplicatesInOrbit orbits@((planet, satellite) : rest) =
  (planet, map snd . filter (\(x, y) -> x == planet) $ orbits) : removeDuplicatesInOrbit (filter (\(x, y) -> x /= planet) rest)

dijkstra :: [String] -> M.Map String Int -> M.Map String [String] -> [String] -> M.Map String Int
dijkstra toVisit weights connections visited =
  if null toVisit
    then weights
    else dijkstra updatedToVisit updatedWeights connections (current : visited)
  where
    current = head toVisit
    connectingNodes = M.lookup current connections
    Just currentWeight = M.lookup current weights
    updatedWeights =
      if isNothing connectingNodes
        then weights
        else M.fromList . map (\(x, y) -> if x `elem` fromJust connectingNodes then (x, min y currentWeight + 1) else (x, y)) . M.toList $ weights
    nextToVisit = let xs = fromMaybe [] connectingNodes in foldr (\x acc -> if x `notElem` visited then x : acc else acc) [] xs
    updatedToVisit = tail toVisit ++ nextToVisit

bfs :: [String] -> M.Map String Int -> M.Map String [String] -> [String] -> String -> Int
bfs toVisit weights connections visited end =
  if null toVisit || current == end
    then currentWeight
    else bfs updatedToVisit updatedWeights connections (current : visited) end
  where
    current = head toVisit
    connectingNodes = M.lookup current connections
    Just currentWeight = M.lookup current weights
    updatedWeights =
      if isNothing connectingNodes
        then weights
        else M.fromList . map (\(x, y) -> if x `elem` fromJust connectingNodes then (x, min y currentWeight + 1) else (x, y)) . M.toList $ weights
    nextToVisit = let xs = fromMaybe [] connectingNodes in foldr (\x acc -> if x `notElem` visited then x : acc else acc) [] xs
    updatedToVisit = tail toVisit ++ nextToVisit

checkSumOfOrbits :: M.Map String Int -> Int
checkSumOfOrbits = M.foldl (+) 0

findPlanetFromSatellite :: (Eq b) => b -> [(a, b)] -> Maybe a
findPlanetFromSatellite satellite = foldl (\acc (x, y) -> if y == satellite then Just x else acc) Nothing

-- PART 1
main :: IO ()
main = do
  putStrLn "Enter file name"
  fileName <- getLine
  fileContents <- readFile fileName
  let Just parsedContent = runParser newLineSepOrbitParser fileContents
  let orbits = map (\orbit -> (planet orbit, satellite orbit)) $ fst parsedContent
  let allPlanets = S.toList $ S.fromList $ concatMap (\orbit -> [planet orbit, satellite orbit]) $ fst parsedContent
  let orbitsMap = M.fromList $ removeDuplicatesInOrbit orbits
  let weights = let w = M.fromList $ map (,maxBound :: Int) allPlanets in M.insert "COM" 0 w
  let newWeights = dijkstra ["COM"] weights orbitsMap []
  print $ checkSumOfOrbits newWeights

-- PART 2
-- main :: IO ()
-- main = do
--   putStrLn "Enter file name"
--   fileName <- getLine
--   fileContents <- readFile fileName
--   let Just parsedContent = runParser newLineSepOrbitParser fileContents
--   let orbits = concatMap (\orbit -> let p = planet orbit; s = satellite orbit in [(p, s), (s, p)]) $ fst parsedContent
--   let allPlanets = S.toList $ S.fromList $ concatMap (\orbit -> [planet orbit, satellite orbit]) $ fst parsedContent
--   let orbitsMap = M.fromList $ removeDuplicatesInOrbit orbits
--   let myPlanet = fromMaybe "" $ findPlanetFromSatellite "YOU" orbits
--   let santaPlanet = fromMaybe "" $ findPlanetFromSatellite "SAN" orbits
--   let weights = let w = M.fromList $ map (,maxBound :: Int) allPlanets in M.insert myPlanet 0 w
--   let orbitalTransfers = bfs [myPlanet] weights orbitsMap [] santaPlanet
--   print orbitalTransfers