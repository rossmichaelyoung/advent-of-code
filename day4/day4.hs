module Main where

minVal :: Int
minVal = 146810

maxVal :: Int
maxVal = 612564

twoAdjacent :: String -> Bool
twoAdjacent [] = False
twoAdjacent [x] = False
twoAdjacent (x : y : xs)
  | x == y = null (takeWhile (== x) xs) || twoAdjacent (dropWhile (== x) xs)
  | otherwise = twoAdjacent (y : xs)

onlyIncrease :: String -> Bool
onlyIncrease [] = True
onlyIncrease [x] = True
onlyIncrease (x : y : xs) = ((read [x] :: Int) <= (read [y] :: Int)) && onlyIncrease (y : xs)

meetsCriteria :: String -> Bool
meetsCriteria xs =
  length xs == 6 && val >= minVal && val <= maxVal && twoAdjacent xs && onlyIncrease xs
  where
    val = read xs

main :: IO ()
main = print . length . foldr (\x acc -> if meetsCriteria $ show x then x : acc else acc) [] $ [minVal .. maxVal]