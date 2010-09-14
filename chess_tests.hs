{-# LANGUAGE TypeSynonymInstances, OverlappingInstances, FlexibleInstances #-}
module Chess_Tests  where

import Control.Monad
import Test.QuickCheck


import Chess

instance Arbitrary Color where
	 arbitrary = elements [White, Black]
	 
instance Arbitrary Castling where
	 arbitrary = elements [NoCastling, QueenCastling, KingCastling, BothCastling]	 
	 
instance Arbitrary Kind where
	 arbitrary = elements [King, Queen, Rook, Bishop, Knight, Pawn, Pawn, Pawn, Pawn] --powns are many :)
	 
instance Arbitrary Piece where
	 arbitrary = do
		color <- arbitrary
		kind <- arbitrary
		return (Piece color kind)
		
instance Arbitrary Line where
	arbitrary = do 
		pieces <- vectorOf 8 (arbitrary :: Gen (Maybe Piece))
		return $ Line pieces
		
instance Arbitrary Board where 
	arbitrary = do
		lines <- vectorOf 8 (arbitrary :: Gen Line)
		return $ Board lines
		
instance Arbitrary Position where 
	arbitrary = do		
		board <- arbitrary
		nextToMove <- arbitrary
		whiteCastling <- arbitrary
		blackCastling <- arbitrary
		let arbSquere = do
			row <- choose (0,7) :: Gen Int
			column <- choose (0, 7) :: Gen Int
			oneof [return Nothing, return $ Just (row, column)]				
		enPassant <- arbSquere
		halfMoves <- choose (0, 50) :: Gen Int
		moves <- choose (0, 150) :: Gen Int
		return $ Position board nextToMove whiteCastling blackCastling enPassant halfMoves moves
		

		
		

	