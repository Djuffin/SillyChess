module FEN (renderFEN, readFEN, readFENorError) where

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
				renderSquere (row, column) = (['a' .. 'h'] !! column) : (show $ 1 + row)
				halfMoveStr = show halfMovesSinceAction
				movesStr = show fullMoves
				
readFEN :: String -> Maybe Position
readFEN fen = case readFENorError fen of
				Left err -> Nothing
				Right position -> Just position
				
readFENorError :: String -> Either String Position
readFENorError fen = case parse p_position "" fen of
				Left err -> Left $ show err
				Right position -> Right position
				

p_position :: CharParser () Position
p_position = do
				lines <- count 8 p_line
				char ' '
				nextToMove <- p_color
				char ' '
				(wc, bc) <- p_twocastlings
				char ' '
				enPassant <- p_squere
				char ' '			
				halfMove <- p_int
				char ' '
				moves <- p_int
				return (Position (Board $ reverse lines) nextToMove wc bc enPassant halfMove moves)

p_squere :: CharParser () (Maybe Square)
p_squere = do			
			empty <- liftM isJust . optionMaybe $ char '-'
			if empty 
				then return Nothing
				else do 
					column <- oneOf ['a' .. 'h']
					row <- digit
					return $ Just ((digitToInt row) - 1, fromJust $ elemIndex column ['a' .. 'h'])
		
p_twocastlings :: CharParser () (Castling, Castling)
p_twocastlings = do 
					chars <- many1 $ oneOf "-KQkq"					
					let whiteKing = elem 'K' chars
					let whiteQueen = elem 'Q' chars
					let blackKing = elem 'k' chars
					let blackQueen = elem 'q' chars
					let getCastling k q = case (k, q) of
										  (False, False) -> NoCastling
										  (True, False) -> KingCastling
										  (False, True) -> QueenCastling
										  (True, True) -> BothCastling
					return (getCastling whiteKing whiteQueen, getCastling blackKing blackQueen) 

				
p_color :: CharParser () Color
p_color = do
			ch <- oneOf "wb"
			return $ fromJust $ lookup ch [('w', White), ('b', Black)]

p_line :: CharParser () Chess.Line
p_line = do 
			pieces <- many p_piece 
			optional $ char '/'
			return (Chess.Line $ concat pieces)		
			
p_piece :: CharParser () [(Maybe Chess.Piece)]
p_piece = do 
			ch <- oneOf "KQRBNPkqrbnp12345678"
			let piece = lookup ch [('K', (Piece White King)), ('Q', (Piece White Queen)), ('R', (Piece White Rook)), ('B', (Piece White Bishop)), ('N', (Piece White Knight)), ('P', (Piece White Pawn)), 
									('k', (Piece Black King)), ('q', (Piece Black Queen)), ('r', (Piece Black Rook)), ('b', (Piece Black Bishop)), ('n', (Piece Black Knight)), ('p', (Piece Black Pawn))] 
			let emptySqeres = replicate (digitToInt ch) Nothing 
			return $ if isDigit ch then emptySqeres else [piece]
		

	
p_int :: CharParser () Int
p_int = (liftM read) $ many1 digit