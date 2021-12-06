import Utils.Helper
import Data.List (foldl')
import Utils.Parser
import qualified Data.Map as M

inputParser :: Parser [Int]
inputParser = sepByParser commaParser naturalNumParser

createFishMap :: [Int] -> M.Map Int Int
createFishMap fish = foldl' (\acc x -> M.adjust (+1) x acc) initialMap fish
    where initialMap = M.fromList [(x, 0) | x <- [0..8]]

simulateFish :: M.Map Int Int -> Int -> M.Map Int Int
simulateFish fish 0 = fish
simulateFish fish numDays = simulateFish newFish (numDays - 1)
    where newFish = M.foldrWithKey (\key val acc -> if key == 0 then addNewFish acc val else M.adjust (+val) (key - 1) acc) (M.fromList [(x, 0) | x <- [0..8]]) fish
          addNewFish m val = M.adjust (+val) 8 (M.adjust (+val) 6 m)

main = do
    fileContent <- getArgAndReadFile
    let initialFish = let Just (fish,_) = runParser inputParser fileContent in fish
    let fishMap = createFishMap initialFish
    let finalFish = simulateFish fishMap 256
    print $ foldl' (\x acc -> acc+x) 0 finalFish