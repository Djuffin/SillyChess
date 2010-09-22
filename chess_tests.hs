{-# LANGUAGE TypeSynonymInstances, OverlappingInstances, FlexibleInstances #-}
module Chess_Tests  where

import Control.Monad
import Test.QuickCheck
import System.Random
import System.IO
import Data.List
import Data.Char
import Data.Maybe


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

data SqWrapper = SqWrapper Square 
				deriving (Eq, Show)

instance Arbitrary SqWrapper where
	 arbitrary = do 
			row <- choose (0,7) :: Gen Int
			column <- choose (0, 7) :: Gen Int	
			return $ SqWrapper (row, column)


		
--------------- tests themselves -------------
prop_set_get_piece :: Board -> SqWrapper -> Maybe Piece -> Bool
prop_set_get_piece board (SqWrapper sq) piece = piece == (getPieceOfBoard (setPieceOfBoard board sq piece) sq)

prop_get_piece = and [Just (Piece White Rook) == getPieceOfBoard initialBoard (0,0),
	Just (Piece Black Bishop) == getPieceOfBoard initialBoard (7,2),
	Just (Piece White Pawn) == getPieceOfBoard initialBoard (1,3),
	Nothing == getPieceOfBoard initialBoard (2,4)]
	
prop_get_moves_for_pown = 5 == length (map to $ getMoves initialPosition (6, 5))
	--[(5,5), (4,5)] == (map to $ getMoves initialPosition (6, 5))
	--and [ [(2,3), (3,3)] == (map to $ getMoves initialPosition (1, 3)),
		  
	   
	
quickCheckArgs = Args (Just (mkStdGen 1, 1)) 300 1 300
	  
main = do
	quickCheckWith quickCheckArgs prop_get_piece
	quickCheckWith quickCheckArgs prop_set_get_piece
	--quickCheckWith quickCheckArgs prop_get_moves_for_pown

