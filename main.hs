{-# LANGUAGE TypeSynonymInstances, OverlappingInstances #-}
 
module Main (main) where

import System.IO
import Data.List
import Data.Char
import Data.Maybe

import Chess

main = do
	putStrLn $ renderFEN $ initialPosition	
	
	
-- show instances

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