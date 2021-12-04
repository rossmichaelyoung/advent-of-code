import Utils.Parser
  ( Parser (runParser),
    stringParser,
    intParser,
    wsParser
  )
import Utils.Helper
import Data.List (transpose, foldl')

magAndDirectionParser :: Parser (String, Int)
magAndDirectionParser = fmap (,) stringParser <*> (wsParser *> intParser)

parseInput :: [String] -> [(String, Int)]
parseInput xs = map (\x -> let Just (magAndDirection, _) = runParser magAndDirectionParser x in magAndDirection) xs

calculateFinalPosition :: [(String, Int)] -> (Int, Int)
calculateFinalPosition xs = firstTwo . foldl' (\currentPosition magAndDirection -> calculateNewPosition currentPosition magAndDirection) (0,0,0) $ xs
        where 
            calculateNewPosition :: (Int, Int, Int) -> (String, Int) -> (Int, Int, Int)
            calculateNewPosition (x,y,z) (direction, magnitude) | direction == "forward" = (x + magnitude, y + z * magnitude, z)
                                                              | direction == "down" = (x, y, z + magnitude)
                                                              | direction == "up" = (x, y, z - magnitude)
            
            firstTwo :: (Int, Int, Int) -> (Int, Int)
            firstTwo (x,y,z) = (x,y)

multiplyCoordinates :: (Int, Int) -> Int
multiplyCoordinates (x,y) = x*y

main = getArgAndReadFile >>= print . multiplyCoordinates . calculateFinalPosition . parseInput . lines