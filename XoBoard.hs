module XoBoard 
( Symbol (X, O)
, Square (EmptySquare, FilledSquare)
, Board (Board)
, emptyBoard
, possibleThrees
, drawSquare
, drawBoard
, checkHowManyInRow
) where

import Data.List
 
data Symbol = X | O deriving (Eq)
data Square = EmptySquare | FilledSquare Symbol deriving (Eq)
data Board = Board [Square]

emptyBoard = Board (replicate 9 EmptySquare)

-- Zero-based indices (0,1,2 are top horizontal row, 3,4,5 are middle horizontal row...) of all
-- possible "three in a row" combinations.
possibleThrees :: [[Int]]
possibleThrees = [[0,1,2],[3,4,5],[6,7,8],[0,3,6],[1,4,7],[2,5,8],[0,4,8],[2,4,6]] 

drawSquare :: Square -> Char -> Char
drawSquare square emptySquareChar
	| (square == FilledSquare X) = 'x'
	| (square == FilledSquare O) = 'o'
	| otherwise			  = emptySquareChar
		
drawBoard :: Board -> String
drawBoard (Board squares) = (concat . transpose) [(zipWith ($) (map drawSquare squares) "______   "), "||\n||\n||"]

checkHowManyInRow :: Board -> Square -> [Int] -> Int
checkHowManyInRow (Board squares) symbol threeIndices = length $ filter (==symbol) $ map snd $ filter (\(a,b) -> a `elem` threeIndices) (zip [0,1..] squares)
