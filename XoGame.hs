import XoBoard
import XoRobot

import Control.Monad
import Data.Either
import Data.Char
import Data.List
import Data.Maybe

data GameState = InPlay Board | Winner Symbol | Draw

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
		Draw				 -> putStrLn $ "\n" ++ (drawBoard newBoard) ++ "\n\nCat's game." 	 
		Winner winner		 -> putStrLn $ "\n" ++ (drawBoard newBoard) ++ "\n\nPlayer " ++ (if winner == X then "X" else "O") ++ " wins!"


-- Some testing stuff.
testBoard = (Board [FilledSquare X, FilledSquare X, FilledSquare O, EmptySquare, FilledSquare O, EmptySquare, EmptySquare, EmptySquare, FilledSquare O])

-- X X O
-- _ O _
-- _ _ O
