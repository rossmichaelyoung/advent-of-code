module Main where

import Control.Monad

fuelRequirements :: [Integer] -> Integer
fuelRequirements = sum . map (\x -> x `div` 3 - 2)

fuelRequirements' :: [Integer] -> Integer
fuelRequirements' = sum . map (sum . tail . takeWhile (> 0) . iterate (\x -> x `div` 3 - 2))

main :: IO Integer
main = putStrLn "Enter file name" >> getLine >>= (readFile >=> (\fileContents -> return (fuelRequirements' . map read . words $ fileContents)))