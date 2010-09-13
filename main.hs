{-# LANGUAGE TypeSynonymInstances, OverlappingInstances #-}
 
module Main (main) where

import System.IO
import Data.List
import Data.Char

import Chess

main = do
	putStrLn $ show $ initialBoard	
	
	
-- show instances

instance Show Kind where
	show King = "K"
	show Queen = "Q"
	show Rook = "R"
	show Bishop = "B"
	show Knight = "N"
	show Pawn = "P"
	
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