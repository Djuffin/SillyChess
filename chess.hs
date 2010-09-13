module Chess where

data Kind = King | Queen | Rook | Bishop | Knight | Pawn
                 deriving (Eq)
				 
data Color = White | Black
                 deriving (Eq)
				 
data Piece = Piece Color Kind
				deriving (Eq)
				
type Square	= (Int, Int) -- row, column (from zero)

data Line = Line [Maybe Piece]				

data Board = Board [Line] -- rows from 1 to 8

data Castling = None | QueenCastling | KingCastling | Both

data Position = Position {
					board :: Board,
					nextToMove :: Color,
					whiteCastling :: Castling,
					blackCastling :: Castling,
					enPassant :: Maybe Square					
				}

getPieceOfLine :: Line -> Int -> Maybe Piece
getPieceOfLine (Line pieces) n = pieces !! n

getPieceOfBoard :: Board -> Square -> Maybe Piece
getPieceOfBoard (Board rows) (row, column) =  getPieceOfLine (rows !! row) column

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
