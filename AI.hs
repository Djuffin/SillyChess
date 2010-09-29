module AI where

import Data.List
import Data.Char
import Data.Maybe
import System.IO
import System.Random

import Control.Monad
import Control.Parallel
import Control.Parallel.Strategies

import Chess

getBestMove :: Position -> IO Move
getBestMove pos = do
					rnd <- (randomIO :: IO Int)
					let moves = getAllLegalMoves pos
					let evaluateMove move =  evaluateDeep 2 $ applyMove pos move
					let movesWithValue = zip moves $ parMap rwhnf evaluateMove moves
					let criterion = if nextToMove pos == White then maximumBy else minimumBy
					let move = fst $ criterion (\m1 m2 -> compare (snd m1) (snd m2)) movesWithValue
					return move

evaluateDeep :: Int -> Position -> Int
evaluateDeep depth position = 
		let moves = getAllLegalMoves position in
		let evaluator = if depth == 0 then evaluate else evaluateDeep (depth - 1) in 
		let criterion = if nextToMove position == White then maximum else minimum in
		let values = map (evaluator . applyMove position) moves in
		if null moves then evaluate position else criterion values
		
					
					

-------
getAllPiecesOfBoard :: Board -> Color -> [Piece]
getAllPiecesOfBoard brd colorToGet = filter (\p -> colorToGet == color p) $ mapMaybe (getPieceOfBoard  brd) allSquares




-- evaluates position value for White
evaluate :: Position -> Int
evaluate p = (evaluateSide p White) - (evaluateSide p Black)

evaluateSide :: Position -> Color -> Int
evaluateSide p color = (evaluateMaterial p color) + (evaluateCheckmate p color)

evaluateCheckmate :: Position -> Color -> Int
evaluateCheckmate p color = if isCheckmate brd color then -1000000 
							else if isCheck brd color then -10 else 0
			where brd = board p

evaluateMaterial :: Position -> Color -> Int
evaluateMaterial p color = sum $ map evaluatePiece $ getAllPiecesOfBoard (board p) color
			where 
				evaluatePiece (Piece _ Pawn) = 100
				evaluatePiece (Piece _  Bishop) = 300
				evaluatePiece (Piece _  Knight) = 300
				evaluatePiece (Piece _  Rook) = 500
				evaluatePiece (Piece _  Queen) = 900
				evaluatePiece _ = 0