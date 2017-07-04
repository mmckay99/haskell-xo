module XoRobot 
( 
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
