{-# LANGUAGE TypeSynonymInstances, FlexibleInstances  #-}
 
module Main (main) where

import System.IO
import Data.List
import Data.Char
import Data.Maybe

import Chess
import FEN

main = do
	--putStrLn $ renderFEN $ initialPosition
	let fen = "rn3rk1/1b1pbppp/p2q1n2/2pP4/2B5/2N2N2/PP3PPP/R1BQ1RK1 w - - 5 12"
	let textToShow = case readFENorError $  fen of
				Left err -> err
				Right position -> show position	
	putStrLn textToShow
	
	
-- show instances
instance Show Color where
	show White = "w"
	show Black = "b"
	
instance Show Kind where
	show kind = fromJust $ lookup kind [(King, "K"), (Queen, "Q"), (Rook, "R"), (Bishop, "B"), (Knight, "N"), (Pawn, "P")] 


	
instance Show Piece where	
	show (Piece White kind) = show kind
	show (Piece Black kind) = map toLower $ show kind
											   
	
instance Show Line where
	show (Line pieces) = concat $ map showPiece pieces
					where 
						showPiece Nothing = "."
						showPiece (Just p) = show p
					

instance Show Board where
	show (Board lines) = intercalate "\n" $ map show $ reverse lines
	
instance Show Position where
	show p = (show $ board p) ++ "\n " ++ (show $ nextToMove p) ++ " " ++ (show $ whiteCastling p)  ++ " " ++ (show $ blackCastling p) ++ 
				" " ++ (showSquare $ enPassant p) ++ " " ++ (show $ halfMovesSinceAction p) ++ " " ++ (show $ fullMoves p)
					where 
						showSquare Nothing = "-"
						showSquare (Just (row, column)) = (['a' .. 'h'] !! column) : (show $ 1 + column)
