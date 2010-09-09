module Chess where

data Kind = King | Queen | Rook | Bishop | Knight | Pawn
                 deriving (Eq, Show)
				 
data Color = White | Black
                 deriving (Eq, Show)
				 
data Piece = Piece Color Kind
				deriving (Eq, Show)