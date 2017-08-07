module XoRobot 
( robotTurn
, twoAndEmpty
) where

import XoBoard

import Data.List
import Data.Maybe
import Debug.Trace

robotTurn :: Board -> Symbol -> IO (Board)
robotTurn board symbol =
	let rankedPossibleMoves =
			(playsToWin board symbol) ++
			(playsToBlock board symbol) ++
			(playsToFork board symbol) ++
			(playsToBlockOpponentFork board symbol) ++
			(playsToCenter board symbol) ++
			(playsToOppositeCorner board symbol) ++
			(playsToEmptyCorner board symbol) ++
			(playsToEmptySide board symbol)
	in if (null rankedPossibleMoves)
		then
			pure board
		else
			let
				squares = case board of (Board s) -> s
				offset = head rankedPossibleMoves
				splitBoard = (splitAt (offset + 1) squares) 
			in pure $ Board ((init (fst splitBoard)) ++ [FilledSquare symbol] ++ (snd splitBoard)) 

-- These "playsTo..." strategy functions return a list of indices (ranging 0..8) corresponding
-- to possible valid plays based on their own particular strategy (e.g. playing in the centre). 

playsToWin :: Board -> Symbol -> [Int]
playsToWin board symbol = catMaybes $ map (twoAndEmpty board symbol) possibleThrees 

playsToBlock :: Board -> Symbol -> [Int]
playsToBlock board symbol = catMaybes $ map (twoAndEmpty board (opponentSymbol symbol)) possibleThrees

-- We must find all cases where if we were to play there, there would be 2 ways to win.
playsToFork :: Board -> Symbol -> [Int]
playsToFork board symbol = filter (createsFork board symbol) [0..8]


-- Find all plays where if the OPPONENT played, they would have a fork.
playsToBlockOpponentFork :: Board -> Symbol -> [Int]
playsToBlockOpponentFork board symbol = filter (createsFork board (opponentSymbol symbol)) [0..8] 

playsToCenter :: Board -> Symbol -> [Int]
playsToCenter (Board squares) symbol = if (squares !! 4 == EmptySquare) then [4] else []

playsToOppositeCorner :: Board -> Symbol -> [Int]
playsToOppositeCorner (Board squares) symbol =
	let 
		otherSymbol = opponentSymbol symbol
		oppositeCorners = [(0,8),(2,6),(6,2),(8,0)]
		otherPlayerCorners = filter (\i -> squares !! i == (FilledSquare otherSymbol)) [0,2,6,8]
	in map snd $ filter (\(i1, i2) -> i1 `elem` otherPlayerCorners && (squares !! i2 == EmptySquare)) oppositeCorners
		
playsToEmptyCorner :: Board -> Symbol -> [Int]
playsToEmptyCorner (Board squares) symbol = filter (\i -> squares !! i == EmptySquare) [0,2,6,8]

playsToEmptySide :: Board -> Symbol -> [Int]
playsToEmptySide (Board squares) symbol = filter (\i -> squares !! i == EmptySquare) [1,3,5,7]

-- These are utility functions.

-- Takes a board, list of 3 integers indices, and checks if this row in the board contains 
-- two symbols of "symbol" and an empty square.
twoAndEmpty :: Board -> Symbol -> [Int] -> Maybe Int
twoAndEmpty (Board squares) symbol indices = 
	let 
		filledSymbol = (FilledSquare symbol) 
		rowSymbols = map (\i -> squares !! i) indices
	in if length (filledSymbol `elemIndices` rowSymbols) == 2 && (EmptySquare `elem` rowSymbols)
	   then Just $ indices !! (fromJust (EmptySquare `elemIndex` rowSymbols))
	   else Nothing

-- Takes a board, a symbol and an index, and checks if placing a symbol
-- at this index would create a new board where there are two possibilities to win. 
createsFork :: Board -> Symbol -> Int -> Bool
createsFork (Board squares) symbol index =
	let
		splitBoard = (splitAt (index + 1) squares) 
		newBoard = Board ((init (fst splitBoard)) ++ [FilledSquare symbol] ++ (snd splitBoard)) 
		possiblePlaysToWin = playsToWin newBoard symbol
	in (squares !! index == EmptySquare) && (length possiblePlaysToWin) == 2
	
	
				
