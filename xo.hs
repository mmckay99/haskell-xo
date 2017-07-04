import Control.Monad
import Data.Either
import Data.Char
import Data.List

data Symbol = X | O deriving (Eq)
data Square = EmptySquare | FilledSquare Symbol deriving (Eq)
data Board = Board [Square]
data GameState = InPlay Board | Winner Symbol | Draw

emptyBoard = Board (replicate 9 EmptySquare)

drawSquare :: Square -> Char -> Char
drawSquare square emptySquareChar
	| (square == FilledSquare X) = 'x'
	| (square == FilledSquare O) = 'o'
	| otherwise			  = emptySquareChar
		
drawBoard :: Board -> String
drawBoard (Board squares) = (concat . transpose) [(zipWith ($) (map drawSquare squares) "______   "), "||\n||\n||"]

main :: IO()
main = do 
		let turns = (concat $ repeat [(\b -> playerTurn b X), (\b -> robotTurn b O)]) 
		game emptyBoard turns

playerChoice :: Board -> Symbol -> IO (Board)
playerChoice (Board squares) symbol = do
			input <- getLine

			if (length input == 1 && (head input >= 'a') && (head input <= 'i'))
				then do
						let offset = 8 - ((ord 'i') - (ord (head input)))						
						if squares !! offset == EmptySquare 
						then let splitBoard = (splitAt (offset + 1) squares) in
							pure $ Board ((init (fst splitBoard)) ++ [FilledSquare symbol] ++ (snd splitBoard))
						else do
							putStrLn "That square is taken. Enter a new letter."
							playerChoice (Board squares) symbol 
								
			else do
				putStrLn "Enter a valid letter (a-i)."		
				playerChoice (Board squares) symbol
		
				
playerTurn :: Board -> Symbol -> IO (Board)
playerTurn board symbol = do
	putStrLn $ "\n" ++ (drawBoard board) ++ "\n\n" ++ "a|b|c\nd|e|f\ng|h|i\n\nEnter choice:"
	playerChoice board symbol
	
robotTurn :: Board -> Symbol -> IO (Board)
robotTurn board symbol = do
	putStrLn $ "\n" ++ (drawBoard board) ++ "\n\n" ++ "a|b|c\nd|e|f\ng|h|i\n\nEnter choice:"
	playerChoice board symbol

-- Zero-based indices (0,1,2 are top horizontal row, 3,4,5 are middle horizontal row...) of all
-- possible "three in a row" combinations.
possibleThrees :: [[Int]]
possibleThrees = [[0,1,2],[3,4,5],[6,7,8],[0,3,6],[1,4,7],[2,5,8],[0,4,8],[2,4,6]] 

checkHowManyInRow :: Board -> Square -> [Int] -> Int
checkHowManyInRow (Board squares) symbol threeIndices = length $ filter (==symbol) $ map snd $ filter (\(a,b) -> a `elem` threeIndices) (zip [0,1..] squares)

checkGame :: Board -> GameState
checkGame (Board squares)
	| any (==3) (map (checkHowManyInRow (Board squares) (FilledSquare X)) possibleThrees) 	= Winner X
	| any (==3) (map (checkHowManyInRow (Board squares) (FilledSquare O)) possibleThrees) 	= Winner O
	| not (EmptySquare `elem` squares) 														= Draw
	| otherwise 																			= InPlay (Board squares) 
			
game :: Board -> [(Board -> IO (Board))] -> IO()
game board playerFuncs = do
	newBoard <- (head playerFuncs) board
	let gameState = checkGame newBoard

	case gameState of
		(InPlay afterPlayer) -> game afterPlayer (tail playerFuncs)
		Draw				 -> putStrLn $ "\n" ++ (drawBoard newBoard) ++ "Cat's game." 	 
		Winner winner		 -> putStrLn $ "\n" ++ (drawBoard newBoard) ++ "\n\nPlayer " ++ (if winner == X then "X" else "O") ++ " wins!"
