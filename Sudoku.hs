module Sudoku (sudoku) where

n = 3
n2 = n * n


data BoardElement = BoardElement {
      value   :: Integer
    , options ::  [Integer]
}

instance Show BoardElement where
    show (BoardElement 0 options) = " |" ++ (foldl (++) "" [show n | n <- options]) ++ (replicate (n2 - (length options)) ' ')  ++ " "
    show (BoardElement value _) = " " ++ (show value) ++ (replicate n2 ' ') ++ " "

makeBoardElementFromInteger int = BoardElement int options
    where options = case int of
                      0 -> [1..9]
                      _ -> []

data Board = Board {
      elements :: [[BoardElement]]
}
instance Show Board where
    show board = foldl (++) "" [(foldl (++) "" [show el | el <- row]) ++ "\n" | row <- elements board]

makeInitialBoard vals = Board [[makeBoardElementFromInteger i | i <- row] | row <- vals]

boardToList board = [[value el | el <- row] | row <- rows board]

row board rowIdx = (elements board) !! rowIdx
rows board = elements board
col board colIdx = [row !! colIdx | row <- rows board]
cols board = [col board colIdx | colIdx <- [0..n2-1]]

getVal board rowIdx colIdx = (row board rowIdx) !! colIdx

transpose board = Board [[el | el <- col] | col <- cols board]

subsq board subsqIdx = 
    let subsqStartRow = (subsqIdx `div` n) * n
        subsqStartCol = (subsqIdx `mod` n) * n
    in [getVal board (subsqStartRow + r) (subsqStartCol + c) | r <- [0..n-1], c <- [0..n-1]]

subsqs board = [subsq board subsqIdx | subsqIdx <- [0..n2-1]]

makeBoardFromSubsqs subsqs = Board [[getElFromSubsqs rowIdx colIdx | colIdx <- [0..n2-1]] | rowIdx <- [0..n2-1]]
    where getElFromSubsqs r c = let subsqIdx = (r `div` n) * 3 + (c `div` n)
                                    subsqOff = (r `mod` n) * 3 + (c `mod` n)
                                in (subsqs !! subsqIdx) !! subsqOff


reduceElement el used = case (value el) of
                          0 -> let rl = filter (\x -> x `notElem` used) (options el)
                               in 
                                 if length rl == 1 then BoardElement (head rl) [] else BoardElement 0 rl
                          _ -> el

reduceList l = [reduceElement el used | el <- l] 
    where used = filter (\x -> x > 0) [value el | el <- l]

reduceRows board = Board [reduceList row | row <- rows board]
reduceCols board = transpose (Board [reduceList row | row <- rows (transpose board)])
reduceSubsqs board = makeBoardFromSubsqs [ reduceList subsq | subsq <- subsqs board]

reduceBoard1 = reduceRows . reduceCols . reduceSubsqs

reduceBoardN 1 board = reduceBoard1 board
reduceBoardN n board = reduceBoardN (n - 1) (reduceBoard1 board)

sudoku = boardToList . (reduceBoardN 9) . makeInitialBoard

p = [[5,3,0,0,7,0,0,0,0],
     [6,0,0,1,9,5,0,0,0],
     [0,9,8,0,0,0,0,6,0],
     [8,0,0,0,6,0,0,0,3],
     [4,0,0,8,0,3,0,0,1],
     [7,0,0,0,2,0,0,0,6],
     [0,6,0,0,0,0,2,8,0],
     [0,0,0,4,1,9,0,0,5],
     [0,0,0,0,8,0,0,7,9]]


--b = makeInitialBoard p
--r = sudoku p

