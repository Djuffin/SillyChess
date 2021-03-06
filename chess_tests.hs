{-# LANGUAGE TypeSynonymInstances, OverlappingInstances, FlexibleInstances #-}
module Chess_Tests  where

import Control.Monad
import Test.QuickCheck
import System.Random
import System.IO
import Data.List
import Data.Char
import Data.Maybe

import FEN
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
		return $ lineFromList pieces
		
instance Arbitrary Board where 
	arbitrary = do
		lines <- vectorOf 8 (arbitrary :: Gen Line)
		return $ boardFromList lines
		
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
	
prop_check = all (\b -> isCheck b Black) boards
	where 
		fens = [
				"6k1/8/8/3Q4/8/8/8/6K1 w - - 0 1", 
				"6k1/5ppp/5N2/3Q4/8/8/8/6K1 w - - 0 1", 
				"1R4k1/5ppp/8/8/8/8/8/6K1 w - - 0 1 ",
				"6k1/6pp/5p2/8/2B5/8/8/6K1 w - - 0 1", 
				"6k1/5Ppp/5p2/8/5B2/8/8/6K1 w - - 0 1"			
				]
		boards = map (board . fromJust . readFEN) fens
	   
 	   
prop_not_check = not $ any (\b -> isCheck b Black) boards
	where 
		fens = [
				"5k2/5Ppp/5p2/8/5B2/8/8/6K1 w - - 0 1", 
				"5kB1/2RQ1Ppp/3N1p2/8/5B2/8/8/6K1 w - - 0 1", 
				"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
				"rnbqkbnr/8/8/8/8/8/8/RNBQKBNR w KQkq - 0 1 ", 
				"rnbqkbnr/PPP1P1PP/8/8/8/8/8/RNBQKBNR w KQkq - 0 1 "			
				]
		boards = map (board . fromJust . readFEN) fens
		
prop_checkmate = all (\b -> isCheckmate b Black) boards
	where 
		fens = [
				"1R4k1/5ppp/8/8/8/8/8/6K1 w - - 0 1 ",
				"kr6/ppN5/8/8/8/8/8/6K1 w - - 0 1 ",
				"1N6/5K2/7r/2pk4/R3P3/1q6/7B/8 w - - 0 1"
				]
		boards = map (board . fromJust . readFEN) fens		

	
argMany = Args (Just (mkStdGen 1, 1)) 300 1 300 False
argOnce = Args (Just (mkStdGen 1, 1)) 1 1 1 False
	  
main = do
	quickCheckWith argMany prop_get_piece
	quickCheckWith argMany prop_set_get_piece
	quickCheckWith argOnce prop_check
	quickCheckWith argOnce prop_not_check
	quickCheckWith argOnce prop_checkmate


