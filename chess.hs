module Chess where

data Kind = King | Queen | Rook | Bishop | Knight | Pawn
                 deriving (Eq)
				 
data Color = White | Black
                 deriving (Eq)
				 
data Piece = Piece Color Kind
				deriving (Eq)
				
data Square	= Square Int Int -- column, row (from zero)

type Line = [Maybe Piece]				

type Board = [Line] -- from 1 to 9

