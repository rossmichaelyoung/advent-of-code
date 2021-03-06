import Utils.Helper
import Data.List (foldl')
import Utils.Parser
import Control.Applicative
import qualified Data.Map as M

arrowParser :: Parser String
arrowParser = Parser $ \input -> let (token, rest) = span (\x -> x == '-' || x == '>') input in Just (token, rest)

inputParser :: Parser [(Int, Int)]
inputParser = Parser $ \input -> runParser numPair input >>= \(x, input') -> runParser (wsParser *> arrowParser *> wsParser *> numPair) input' >>= \(x', input'') 
                    -> let x1 = head x; y1 = head $ tail x; x2 = head x'; y2 = head $ tail x' in Just ([(x1,y1), (x2, y2)], input'')
    where numPair = sepByParser commaParser naturalNumParser

createVentMap :: [[(Int, Int)]] -> M.Map (Int, Int) Int
createVentMap ventLines = M.fromList [((col, row), 0) | row <- [0..maxRow], col <- [0..maxCol]]
    where maxRow = foldl' (\acc numPair -> let (_,y1) = head numPair; (_,y2) = head $ tail numPair in max acc (max y1 y2)) (-1) ventLines
          maxCol = foldl' (\acc numPair -> let (x1,_) = head numPair; (x2,_) = head $ tail numPair in max acc (max x1 x2)) (-1) ventLines

findAllPointsBetweenPoints :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
findAllPointsBetweenPoints (x1,y1) (x2,y2) | y1 == y2 = [(x,y1) | x <- [(min x1 x2) .. (max x1 x2)]]
                                           | x1 == x2 = [(x1,y) | y <- [(min y1 y2) .. (max y1 y2)]]
                                           | x1 /= x2 && y1 /= y2 = [(x, x * slope + b) | x <- [(min x1 x2) .. (max x1 x2)]]
    where slope = (y2-y1) `div` (x2-x1)
          b = y1 - (slope * x1)

addPointsToMap :: M.Map (Int, Int) Int -> [(Int, Int)] -> M.Map (Int, Int) Int
addPointsToMap ventMap points = foldl' (\acc point -> M.adjust (+1) point acc) ventMap points

main = do
    fileContent <- getArgAndReadFile
    let ventLines = foldl' (\acc line -> let Just (x,_) = runParser inputParser line in x : acc) [] (lines fileContent)
    let ventMap = createVentMap ventLines
    let ventMap' = foldl' (\acc endPoints -> let points = findAllPointsBetweenPoints (head endPoints) (head $ tail endPoints) in addPointsToMap acc points) ventMap ventLines
    let sum = foldl' (\val acc -> if val >= 2 then acc+1 else acc) 0 ventMap'
    print sum
    