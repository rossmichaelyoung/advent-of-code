import qualified Data.Map as M
import qualified Data.Set as S
import Utils.Helper
import Data.List (foldl')
import Utils.Parser
  ( Parser (runParser),
    naturalNumParser,
    sepByParser,
    wsParser,
    commaParser
  )
import Data.Char (isSpace)

bingoNumbersParser :: Parser [Int]
bingoNumbersParser = sepByParser commaParser naturalNumParser

bingoBoardParser :: Parser [Int]
bingoBoardParser = sepByParser wsParser naturalNumParser

formBingoBoards :: [[Int]] -> [[[Int]]]
formBingoBoards xs = [board] ++ (if null rest then [] else formBingoBoards $ tail rest)
        where board = takeWhile (not . null) xs
              rest = dropWhile (not . null) xs

formBingoNumToPosMap :: [[Int]] -> [(Int, (Int, Int))]
formBingoNumToPosMap board = fst $ foldl' (\(acc, rowNum) row -> let (mappedRow,_) = foldl' (\(acc', colNum) bingoNum -> ((bingoNum, (rowNum, colNum)) : acc', colNum+1)) ([], 0) row in (mappedRow ++ acc, rowNum+1)) ([], 0) board

formPosToBingoNumMap :: [[Int]] -> [((Int, Int), Int)]
formPosToBingoNumMap board = fst $ foldl' (\(acc, rowNum) row -> let (mappedRow,_) = foldl' (\(acc', colNum) bingoNum -> (((rowNum, colNum), bingoNum) : acc', colNum+1)) ([], 0) row in (mappedRow ++ acc, rowNum+1)) ([], 0) board

findFirstWinningBoard :: [(M.Map Int (Int, Int), Int)] -> M.Map ((Int, Int), Int) Bool -> [Int] -> (Int, Int, M.Map ((Int, Int), Int) Bool)
findFirstWinningBoard mappedBoards markedBoards [] = (-1, -1, markedBoards)
findFirstWinningBoard mappedBoards markedBoards bingoNumbers = if winningBoardNum > -1 then (winningBoardNum, currNum, markedBoards') else findFirstWinningBoard mappedBoards markedBoards' (tail bingoNumbers)
        where currNum = head bingoNumbers
              (markedBoards', winningBoardNum) = Data.List.foldl' (\(acc, winningBoardNum) (m, boardNum) -> if winningBoardNum > -1 then (acc, winningBoardNum) else if (M.member currNum m) then let Just (pos) = M.lookup currNum m in if (winningBoard pos boardNum acc) then ((M.adjust (\x -> True) (pos, boardNum) acc), boardNum) else ((M.adjust (\x -> True) (pos, boardNum) acc), -1) else (acc, -1)) (markedBoards, -1) mappedBoards
              winningBoard (row, col) boardNum markedBoards'' = foldl' (\acc currCol -> let Just (marked) = M.lookup ((row, currCol), boardNum) markedBoards'' in if currCol == col || marked then acc && True else acc && False) True [0..4] || foldl' (\acc currRow -> let Just (marked) = M.lookup ((currRow, col), boardNum) markedBoards'' in if currRow == row || marked then acc && True else acc && False) True [0..4] -- check rest of row and rest of column from this pos and boardNum to see if bingo

findLastWinningBoard :: [(M.Map Int (Int, Int), Int)] -> M.Map ((Int, Int), Int) Bool -> S.Set Int -> [Int] -> (Int, Int, M.Map ((Int, Int), Int) Bool)
findLastWinningBoard mappedBoards markedBoards boardSet [] = (-1, -1, markedBoards)
findLastWinningBoard mappedBoards markedBoards boardSet bingoNumbers = if winningBoardNum > -1  && S.size boardSet' == 0 then (winningBoardNum, currNum, markedBoards') else findLastWinningBoard mappedBoards markedBoards' boardSet' (tail bingoNumbers)
        where currNum = head bingoNumbers
              (markedBoards', boardSet', winningBoardNum) = Data.List.foldl' (\(markedBoardsAcc, boardSetAcc, winningBoardNum) (m, boardNum) -> if winningBoardNum > -1 && S.size boardSetAcc == 0 then (markedBoardsAcc, boardSetAcc, winningBoardNum) else if (M.member currNum m) then let Just (pos) = M.lookup currNum m in if (winningBoard pos boardNum markedBoardsAcc) then ((M.adjust (\x -> True) (pos, boardNum) markedBoardsAcc), (S.delete boardNum boardSetAcc), boardNum) else ((M.adjust (\x -> True) (pos, boardNum) markedBoardsAcc), boardSetAcc, -1) else (markedBoardsAcc, boardSetAcc, -1)) (markedBoards, boardSet, -1) mappedBoards
              winningBoard (row, col) boardNum markedBoards'' = foldl' (\acc currCol -> let Just (marked) = M.lookup ((row, currCol), boardNum) markedBoards'' in if currCol == col || marked then acc && True else acc && False) True [0..4] || foldl' (\acc currRow -> let Just (marked) = M.lookup ((currRow, col), boardNum) markedBoards'' in if currRow == row || marked then acc && True else acc && False) True [0..4] -- check rest of row and rest of column from this pos and boardNum to see if bingo

sumUnMarked :: Int -> M.Map ((Int, Int), Int) Bool -> M.Map (Int, Int) Int -> Int
sumUnMarked winningBoard markedBoards mappedWinningBoard = Data.List.foldl' (\acc pos -> let Just (marked) = M.lookup (pos, winningBoard) markedBoards in if not marked then let Just (val) = M.lookup pos mappedWinningBoard in acc+val else acc) 0 rowsAndCols
        where rowsAndCols :: [(Int, Int)]
              rowsAndCols = [(row, col) | row <- [0..4], col <- [0..4]]

main = do
    fileContent <- getArgAndReadFile
    let fileLines = lines fileContent
    let bingoNumbers = let Just (nums,_) = runParser bingoNumbersParser (head fileLines) in nums
    let boards = formBingoBoards . map (\board -> let Just (nums,_) = runParser bingoBoardParser board in nums) . map (\line -> if length line == 0 then line else if isSpace $ head line then tail line else line) . tail . tail $ fileLines
    let mappedBoards = fst $ foldl' (\(acc, boardNum) board -> ((M.fromList (formBingoNumToPosMap board), boardNum) : acc, boardNum+1)) ([], 0) boards -- [(map of (bingoNum, pos), boardNum)]
    let markedBoards = M.fromList $ [(((row, col), board), False) | row <- [0..4], col <- [0..4], board <- [0..(length mappedBoards)-1]] -- map of ((pos, boardNum), Boolean)
    let boardSet = S.fromList [0..(length boards)-1]
    let (winningBoard, winningNum, markedBoards') = findLastWinningBoard mappedBoards markedBoards boardSet bingoNumbers
    print winningBoard
    print winningNum
    let mappedWinningBoard = M.fromList (formPosToBingoNumMap $ boards !! winningBoard)
    let unmarkedSum = sumUnMarked winningBoard markedBoards' mappedWinningBoard
    print (unmarkedSum * winningNum)