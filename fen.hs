module FEN (renderFEN, readFEN) where

import Data.List
import Data.Char
import Data.Maybe

--import Text.Regex.Posix
import Text.ParserCombinators.Parsec
import Control.Monad


import Chess

--Forsyth–Edwards Notation
renderFEN :: Position -> String
renderFEN (Position (Board lines) nextToMove whiteCastling blackCastling enPassant halfMovesSinceAction fullMoves) = 
			piecesStr ++ " " ++ (renderNextToMove nextToMove)++ " " ++ (renderCastlings whiteCastling blackCastling) ++ " " ++ (renderEnPass enPassant) ++ " " ++ halfMoveStr ++ " " ++ movesStr
			where
				piecesStr = intercalate "/" $ map renderLine $ reverse lines
				renderLine (Line pieces) = foldEmptySqueres $ map renderPiece pieces
				foldEmptySqueres (d:'1':xs) | isDigit d = (foldEmptySqueres $ (succ d):xs)
				foldEmptySqueres (x:xs) = x:(foldEmptySqueres xs)
				foldEmptySqueres [] = []
				renderPiece Nothing = '1'
				renderPiece (Just (Piece White kind)) = renderKind kind
				renderPiece (Just (Piece Black kind)) = toLower $ renderKind kind
				renderKind kind = fromJust $ lookup kind [(King, 'K'), (Queen, 'Q'), (Rook, 'R'), (Bishop, 'B'), (Knight, 'N'), (Pawn, 'P')] 
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
				
readFEN :: String -> Maybe Position
readFEN fen = let parts = words fen
				in Nothing
	

p_line :: CharParser () Chess.Line
p_line = do 
			pieces <- manyTill p_piece (try $ (char '/' <|> space))
			return (Chess.Line pieces)		
			
p_piece :: CharParser () (Maybe Chess.Piece)
p_piece = do 
			ch <- oneOf "KQRBNPkqrbnp"
			let piece = lookup ch [('K', (Piece White King)), ('Q', (Piece White Queen)), ('R', (Piece White Rook)), ('B', (Piece White Bishop)), ('N', (Piece White Knight)), ('P', (Piece White Pawn)), 
									('k', (Piece Black King)), ('q', (Piece Black Queen)), ('r', (Piece Black Rook)), ('b', (Piece Black Bishop)), ('n', (Piece Black Knight)), ('p', (Piece Black Pawn))] 
			return $ piece
		

	
p_int :: CharParser () Int
p_int = (liftM read) $ many1 digit