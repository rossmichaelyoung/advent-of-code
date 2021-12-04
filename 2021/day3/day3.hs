import Utils.Helper
import Data.Char (digitToInt)
import Data.List (transpose, foldl')

binaryStringtoDecimal :: String -> Int
binaryStringtoDecimal = fst . foldr (\x (binary, pos) -> (digitToInt x * 2 ^ pos + binary, pos+1)) (0,0)

findMostAndLeastAtEachPos :: [String] -> Int -> [(Int, Int)]
findMostAndLeastAtEachPos xs len = foldl' accumulate init xs
        where init :: [(Int, Int)]
              init = [(0,0) | x <- [1..len]]

              accumulate :: [(Int, Int)] -> String -> [(Int, Int)]
              accumulate ((x0, x1):xs) (y:ys) | y == '1' = (x0, x1+1) : accumulate xs ys
                                              | y == '0' = (x0+1, x1) : accumulate xs ys
              accumulate [] ys = []
              accumulate xs [] = []

findGammaAndEpsilon :: [String] -> Int
findGammaAndEpsilon xs = let (epsilon, gamma) = foldl' gammaAndEpsilon ("","") (findMostAndLeastAtEachPos xs (length $ head xs)) in binaryStringtoDecimal epsilon * binaryStringtoDecimal gamma
        where gammaAndEpsilon (epsilon, gamma) (x0,x1) | x0 > x1 = (epsilon ++ "1", gamma ++ "0")
                                                       | x1 >= x0 = (epsilon ++ "0", gamma ++ "1")

findMostAtHead :: [String] -> Char
findMostAtHead xs = let (x0, x1) = foldl' (\(x,y) z -> if z == '0' then (x+1, y) else (x, y+1)) (0,0) (head $ transpose xs) in if x1 >= x0 then '1' else '0'

findLeastAtHead :: [String] -> Char
findLeastAtHead xs = if x1 < x0 then '1' else '0'
        where (x0', x1') = foldl' (\(x,y) z -> if z == '0' then (x+1, y) else (x, y+1)) (0,0) (head $ transpose xs)
              x0 = if x0' == 0 then maxBound :: Int else x0'
              x1 = if x1' == 0 then maxBound :: Int else x1'

findOxygen :: [String] -> String
findOxygen [""] = ""
findOxygen xs = mostFreq : findOxygen (foldl' (\acc x -> if head x == mostFreq then acc ++ [tail x] else acc) [] xs)
        where mostFreq = findMostAtHead xs

findCO2 :: [String] -> String
findCO2 [""] = ""
findCO2 xs = leastFreq : findCO2 (foldl' (\acc x -> if head x == leastFreq then acc ++ [tail x] else acc) [] xs)
        where leastFreq = findLeastAtHead xs

findOxygenAndCO2 :: [String] -> Int
findOxygenAndCO2 xs = binaryStringtoDecimal oxygen * binaryStringtoDecimal co2
        where oxygen = findOxygen xs
              co2 = findCO2 xs

-- question 1
-- main = getArgAndReadFile >>= print . findGammaAndEpsilon . lines

-- question 2
main = getArgAndReadFile >>= print . findOxygenAndCO2 . lines