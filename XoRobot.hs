module XoRobot 
( robotTurn
) where

import XoBoard

import Data.List
import Data.Maybe

-- These functions find patterns in the board.

-- Takes an "annotated square", i.e. tuple of ([i1, i2, i3], [s1, s2, s3]) where i is an index 0..8 and s 
-- is the type of square at the corresponding index. Returns true if there are two 
twoSymbolsOneEmpty :: Square -> ([Int], [Square]) -> Bool
twoSymbolsOneEmpty squareType square = ((length $ filter (==squareType) squareTypes) == 2) 
										&& ((length $ filter (==EmptySquare) squareTypes) == 1)
	where squareTypes = snd square

-- Returns the position of an empty square that will make 3 in a row.
findMakeThreeInRow :: Board -> Square -> (Maybe Int)
findMakeThreeInRow (Board squares) squareType = 
	case twoInRowWithEmptys of
		[] 		-> Nothing
		(row:_) -> Just ((fst row) !! (fromJust $ elemIndex EmptySquare (snd row)))
	where
		twoInRowWithEmptys = filter (twoSymbolsOneEmpty squareType) annotatedBoard
		annotatedBoard = zip possibleThrees (map (\t -> (map (\s -> squares !! s) t) ) possibleThrees)

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

-- These strategy functions return a list of indices (ranging 0..8) corresponding
-- to possible valid plays based on their own particular strategy (e.g. playing in the centre). 

playsToWin :: Board -> Symbol -> [Int]
playsToWin board symbol = []

playsToBlock :: Board -> Symbol -> [Int]
playsToBlock board symbol = []

playsToFork :: Board -> Symbol -> [Int]
playsToFork board symbol = []

playsToBlockOpponentFork :: Board -> Symbol -> [Int]
playsToBlockOpponentFork board symbol = []

playsToCenter :: Board -> Symbol -> [Int]
playsToCenter board symbol = if (squares !! 4 == EmptySquare) then [4] else []

playsToOppositeCorner :: Board -> Symbol -> [Int]
playsToOppositeCorner board symbol = []

playsToEmptyCorner :: Board -> Symbol -> [Int]
playsToEmptyCorner (Board squares) symbol = filter (\i -> squares !! i == EmptySquare) [0,2,6,8]

playsToEmptySide :: Board -> Symbol -> [Int]
playsToEmptySide (Board squares) symbol = filter (\i -> squares !! i == EmptySquare) [1,3,5,7]
