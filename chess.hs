module Chess where

import Data.List
import Data.Char
import Data.Maybe

data Kind = King | Queen | Rook | Bishop | Knight | Pawn
                 deriving (Eq)
				 
data Color = White | Black
                 deriving (Eq)
				 
data Piece = Piece { color :: Color, kind :: Kind }
				deriving (Eq)
				
type Square	= (Int, Int) -- row, column (from zero)

data Line = Line [Maybe Piece]				
				deriving (Eq)

data Board = Board [Line] -- rows from 1 to 8
				deriving (Eq)

data Castling = NoCastling | QueenCastling | KingCastling | BothCastling
				deriving (Eq, Show)

data Position = Position {
					board :: Board,
					nextToMove :: Color,
					whiteCastling :: Castling,
					blackCastling :: Castling,
					enPassant :: Maybe Square,
					halfMovesSinceAction :: Int,
					fullMoves :: Int
				}
				deriving (Eq)

initialBoard = Board [
					(Line [Just (Piece White Rook), Just (Piece White Knight), Just (Piece White Bishop), Just (Piece White Queen), Just (Piece White King), Just (Piece White Bishop), Just (Piece White Knight), Just (Piece White Rook)]),
					(Line [Just (Piece White Pawn), Just (Piece White Pawn), Just (Piece White Pawn), Just (Piece White Pawn), Just (Piece White Pawn), Just (Piece White Pawn), Just (Piece White Pawn), Just (Piece White Pawn)]),
					(Line [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]),
					(Line [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]),
					(Line [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]),
					(Line [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]),
					(Line [Just (Piece Black Pawn), Just (Piece Black Pawn), Just (Piece Black Pawn), Just (Piece Black Pawn), Just (Piece Black Pawn), Just (Piece Black Pawn), Just (Piece Black Pawn), Just (Piece Black Pawn)]),
					(Line [Just (Piece Black Rook), Just (Piece Black Knight), Just (Piece Black Bishop), Just (Piece Black Queen), Just (Piece Black King), Just (Piece Black Bishop), Just (Piece Black Knight), Just (Piece Black Rook)])
				]

initialPosition = Position initialBoard White BothCastling BothCastling Nothing 0 1

data Move = Move {
				from :: Square,
				to :: Square,
				piece :: Piece
			}
			| CastleToKingSide
			| CastleToQueenSide

			
---------------- functions ----------------

getPieceOfLine :: Line -> Int -> Maybe Piece
getPieceOfLine (Line pieces) n = pieces !! n

getPieceOfBoard :: Board -> Square -> Maybe Piece
getPieceOfBoard (Board rows) (row, column) =  getPieceOfLine (rows !! row) column

isInBoard :: Square -> Bool
isInBoard (row, column) = and [row > 0, column > 0, row < 8, column < 8]

isOccupied :: Board -> Square -> Bool
isOccupied board  = isJust . (getPieceOfBoard board)


pieceMovesGenerator :: Piece -> Square -> [[Square]]

pieceMovesGenerator (Piece White Pawn) (row, column) = 
	if row == 1 then [[(2, column), (3, column)]]
				else [[(row + 1, column)]]
pieceMovesGenerator (Piece Black Pawn) (row, column) = 
	if row == 6 then [[(5, column), (4, column)]]
				else [[(row - 1, column)]]
				
pieceMovesGenerator (Piece _ King) (row, column) = [[(row, column + 1)], [(row + 1, column + 1)], [(row + 1, column)], [(row + 1, column - 1)], 
	[(row, column - 1)], [(row - 1, column - 1)], [(row - 1, column)], [(row - 1, column + 1)]]

pieceMovesGenerator (Piece _ Knight) (row, column) = [[(row + 1, column + 2)], [(row - 1, column + 2)], [(row + 2, column + 1)], [(row - 2, column + 1)],
	[(row + 1, column - 2)], [(row - 1, column - 2)], [(row + 2, column - 1)], [(row - 2, column - 1)]]
	
pieceMovesGenerator (Piece _ Bishop) (row, column) = [ 
	map (\n -> (row + n, column + n)) [1..],
	map (\n -> (row - n, column + n)) [1..],
	map (\n -> (row - n, column - n)) [1..],
	map (\n -> (row + n, column - n)) [1..]]
	
pieceMovesGenerator (Piece _ Rook) (row, column) = [ 
	map (\n -> (row + n, column)) [1..],
	map (\n -> (row - n, column)) [1..],
	map (\n -> (row, column + n)) [1..],
	map (\n -> (row, column - n)) [1..]]
	
pieceMovesGenerator (Piece color Queen) sq = pieceMovesGenerator (Piece color Bishop) sq ++ pieceMovesGenerator (Piece color Rook) sq

pawnKillsGenerator :: Piece -> Square -> [Square]
pawnKillsGenerator (Piece White Pawn) (row, column) = [(row + 1, column + 1), (row + 1, column - 1)]
pawnKillsGenerator (Piece Black Pawn) (row, column) = [(row - 1, column + 1), (row - 1, column - 1)]


getMoves :: Position -> Square -> [Move]	
getMoves position sq =
	let mbPiece = getPieceOfBoard (board position) sq in
	let	ntm = nextToMove position in
	case mbPiece of 
		Just piece@(Piece nextToMove Pawn) -> undefined
		Just piece@(Piece nextToMove kind) -> undefined
		otherwise -> []


getLegalMoves :: Position -> Square -> [Move]
getLegalMoves position sq = filter isLegalMove moves 
								where 
									moves = getMoves position sq
									isLegalMove move = not $ isCheck (board $ applyMove position move) (nextToMove position)
		
applyMove :: Position -> Move -> Position
applyMove = undefined
		
isCheck :: Board -> Color -> Bool
isCheck = undefined

