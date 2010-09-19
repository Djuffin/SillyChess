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
	let fen = "PrnPBkQK/pkK2RP1/P2R3p/PprpQ2K/1pbP1K2/2R1bppr/2ppPQ1P/KpQp1Ppb w KQq c3 50 57"
	let p1 = fromJust $ readFEN fen
	let t1 = renderFEN p1
	let p2 = fromJust $ readFEN t1
	let t2 = renderFEN p2
	putStrLn $ show p2
	putStrLn t2

	
	


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
