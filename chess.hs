module Chess where

import Data.List
import Data.Char
import Data.Maybe

data Kind = King | Queen | Rook | Bishop | Knight | Pawn
                 deriving (Eq)
				 
data Color = White | Black
                 deriving (Eq)
				 
data Piece = Piece Color Kind
				deriving (Eq)
				
type Square	= (Int, Int) -- row, column (from zero)

data Line = Line [Maybe Piece]				

data Board = Board [Line] -- rows from 1 to 8

data Castling = NoCastling | QueenCastling | KingCastling | BothCastling
				deriving (Eq)

data Position = Position {
					board :: Board,
					nextToMove :: Color,
					whiteCastling :: Castling,
					blackCastling :: Castling,
					enPassant :: Maybe Square,
					halfMovesSinceAction :: Int,
					fullMoves :: Int
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

initialPosition = Position initialBoard White BothCastling BothCastling Nothing 0 1

--Forsyth–Edwards Notation
renderFEN :: Position -> String
renderFEN (Position (Board lines) nextToMove whiteCastling blackCastling enPassant halfMovesSinceAction fullMoves) = 
			piecesStr ++ " " ++ (renderNextToMove nextToMove)++ " " ++ (renderCastlings whiteCastling blackCastling) ++ " " ++ (renderEnPass enPassant) ++ " " ++ halfMoveStr ++ " " ++ movesStr
			where
				piecesStr = intercalate "/" $ map renderLine $ reverse lines
				renderLine (Line pieces) = foldEmptySqueres $ concat $ map renderPiece pieces
				foldEmptySqueres (d:'1':xs) | isDigit d = (foldEmptySqueres $ (succ d):xs)
				foldEmptySqueres (x:xs) = x:(foldEmptySqueres xs)
				foldEmptySqueres [] = []
				renderPiece Nothing = "1"
				renderPiece (Just (Piece White kind)) = renderKind kind
				renderPiece (Just (Piece Black kind)) = map toLower $ renderKind kind
				renderKind kind = fromJust $ lookup kind [(King, "K"), (Queen, "Q"), (Rook, "R"), (Bishop, "B"), (Knight, "N"), (Pawn, "P")] 
				renderNextToMove White = "w"
				renderNextToMove Black = "b"
				renderCastlings NoCastling NoCastling = "-"
				renderCastlings white black = (renderOneCastling white) ++ (map toLower $ renderOneCastling black)
				renderOneCastling castling = fromJust $ lookup castling [(BothCastling ,"KQ"), (KingCastling, "K"), (QueenCastling, "Q"), (NoCastling, "")]
				renderEnPass Nothing = "-" 
				renderEnPass (Just sq) = renderSquere sq
				renderSquere (row, column) = (['a' .. 'h'] !! column) : (show $ 1 + column)
				halfMoveStr = show halfMovesSinceAction
				movesStr = show fullMoves

				

