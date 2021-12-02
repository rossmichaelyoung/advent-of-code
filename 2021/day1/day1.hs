findNumIncreases :: [Int] -> Int
findNumIncreases xs = fst . foldl (\(acc, prev) curr -> if prev < curr then (acc+1, curr) else (acc, curr)) (0, head xs) $ xs

slidingWindowNumIncreases :: [Int] -> Int
slidingWindowNumIncreases xs = tripletFst . foldl (\(acc, prevSum, [a,b,c]) currVal -> if prevSum - a + currVal > prevSum then (acc+1, prevSum - a + currVal, [b,c,currVal]) else (acc, prevSum - a + currVal, [b,c,currVal])) (0, sum initialWindow, initialWindow) . drop 3 $ xs
        where initialWindow = take 3 xs

tripletFst :: (Int, Int, [Int]) -> Int
tripletFst (a,b,c) = a

stringToInt :: String -> Int
stringToInt s = read s :: Int

main = readFile "input.txt" >>= print . slidingWindowNumIncreases . map stringToInt . lines
