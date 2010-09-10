{-# LANGUAGE TypeSynonymInstances, OverlappingInstances #-}
 
module Main (main) where

import System.IO
import Data.List

import Chess

main = do
	putStrLn $ show $ [Just (Piece White Rook), Just (Piece Black Queen), Nothing] 
	
	
-- show instances

instance Show Kind where
	show King = "K"
	show Queen = "Q"
	show Rook = "R"
	show Bishop = "B"
	show Knight = "N"
	show Pawn = "P"
	
instance Show Piece where	
	show (Piece color kind) = show kind
	
instance Show Line where
	show line = concat . intersperse ", " . map showSquare line
					where 
						showSquare Nothing = " "
						showSquare (Just p) = show p
					
	