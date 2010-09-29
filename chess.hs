module Chess where

import Data.List
import Data.Char
import Data.Maybe

import Data.Map (Map)
import qualified Data.Map as Map

data Kind = King | Queen | Rook | Bishop | Knight | Pawn
                 deriving (Eq, Ord)
				 
data Color = White | Black
                 deriving (Eq)
				 
data Piece = Piece { color :: Color, kind :: Kind }
				deriving (Eq)
				
type Square	= (Int, Int) -- row, column (from zero)

newtype Line = Line (Maybe Piece, Maybe Piece, Maybe Piece, Maybe Piece, Maybe Piece, Maybe Piece, Maybe Piece, Maybe Piece)
				deriving (Eq)

newtype Board = Board (Line, Line, Line, Line, Line, Line, Line, Line)
				deriving (Eq)

data Castling = NoCastling | QueenCastling | KingCastling | BothCastling
				deriving (Eq, Show)

data Position = Position {
					board :: Board,
					nextToMove :: Color,
					whiteCastling :: Castling,
					blackCastling :: Castling,
					enPassant :: Maybe Square,
					halfMovesSinceAction :: Int,
					fullMoves :: Int
				}
				deriving (Eq)

emptyLine = Line (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
emptyBoard = Board (emptyLine, emptyLine, emptyLine, emptyLine, emptyLine, emptyLine, emptyLine, emptyLine)
				
initialBoard = Board (
					(Line (Just (Piece White Rook), Just (Piece White Knight), Just (Piece White Bishop), Just (Piece White Queen), Just (Piece White King), Just (Piece White Bishop), Just (Piece White Knight), Just (Piece White Rook))),
					(Line (Just (Piece White Pawn), Just (Piece White Pawn), Just (Piece White Pawn), Just (Piece White Pawn), Just (Piece White Pawn), Just (Piece White Pawn), Just (Piece White Pawn), Just (Piece White Pawn))),
					emptyLine,
					emptyLine,
					emptyLine,
					emptyLine,
					(Line (Just (Piece Black Pawn), Just (Piece Black Pawn), Just (Piece Black Pawn), Just (Piece Black Pawn), Just (Piece Black Pawn), Just (Piece Black Pawn), Just (Piece Black Pawn), Just (Piece Black Pawn))),
					(Line (Just (Piece Black Rook), Just (Piece Black Knight), Just (Piece Black Bishop), Just (Piece Black Queen), Just (Piece Black King), Just (Piece Black Bishop), Just (Piece Black Knight), Just (Piece Black Rook)))
				)
		
initialPosition = Position initialBoard White BothCastling BothCastling Nothing 0 1
emptyPosition = Position emptyBoard White NoCastling NoCastling Nothing 0 1

allSquares = [(i,j) | i <- [0..7], j <- [0..7]]

data Move = Move {
				from :: Square,
				to :: Square,
				piece :: Piece,
				promotion :: Maybe Kind
			}
			| CastleToKingSide
			| CastleToQueenSide 
			deriving (Eq)

-- show instances
instance Show Move where
	show CastleToKingSide = "O-O-O"
	show CastleToQueenSide = "O-O"
	show (Move from to piece promotion) = show piece ++ " " ++ showSquare from ++ "-" ++ showSquare to
								where showSquare  (row, column) = (['a' .. 'h'] !! column) : (show $ 1 + row)

instance Show Color where
	show White = "w"
	show Black = "b"
	
instance Show Kind where
	show kind = fromJust $ lookup kind [(King, "K"), (Queen, "Q"), (Rook, "R"), (Bishop, "B"), (Knight, "N"), (Pawn, "P")] 
	
instance Show Piece where	
	show (Piece White kind) = show kind
	show (Piece Black kind) = map toLower $ show kind
											   
	
instance Show Line where
	show line = concat $ map showPiece $ lineToList line
					where 
						showPiece Nothing = "."
						showPiece (Just p) = show p
					

instance Show Board where
	show board = intercalate "\n" $ map show $ reverse $ boardToList board
	
instance Show Position where
	show p = (show $ board p) ++ "\n " ++ (show $ nextToMove p) ++ " " ++ (show $ whiteCastling p)  ++ " " ++ (show $ blackCastling p) ++ 
				" " ++ (showSquare $ enPassant p) ++ " " ++ (show $ halfMovesSinceAction p) ++ " " ++ (show $ fullMoves p)
					where 
						showSquare Nothing = "-"
						showSquare (Just (row, column)) = (['a' .. 'h'] !! column) : (show $ 1 + column)			
			
---------------- functions ----------------

inverseColor White = Black
inverseColor Black = White

lineFromList :: [Maybe Piece] -> Line
lineFromList (p0:(p1:(p2:(p3:(p4:(p5:(p6:(p7:[])))))))) = Line (p0, p1, p2, p3, p4, p5, p6, p7)

lineToList :: Line -> [Maybe Piece]
lineToList (Line (p0, p1, p2, p3, p4, p5, p6, p7)) = [p0, p1, p2, p3, p4, p5, p6, p7]

boardFromList :: [Line] -> Board
boardFromList (l0:(l1:(l2:(l3:(l4:(l5:(l6:(l7:[])))))))) = Board (l0, l1, l2, l3, l4, l5, l6, l7)

boardToList :: Board -> [Line]
boardToList (Board (l0, l1, l2, l3, l4, l5, l6, l7)) = [l0, l1, l2, l3, l4, l5, l6, l7]

getPieceOfLine :: Line -> Int -> Maybe Piece
getPieceOfLine (Line (s0, s1, s2, s3, s4, s5, s6, s7)) 0 = s0
getPieceOfLine (Line (s0, s1, s2, s3, s4, s5, s6, s7)) 1 = s1
getPieceOfLine (Line (s0, s1, s2, s3, s4, s5, s6, s7)) 2 = s2
getPieceOfLine (Line (s0, s1, s2, s3, s4, s5, s6, s7)) 3 = s3
getPieceOfLine (Line (s0, s1, s2, s3, s4, s5, s6, s7)) 4 = s4
getPieceOfLine (Line (s0, s1, s2, s3, s4, s5, s6, s7)) 5 = s5
getPieceOfLine (Line (s0, s1, s2, s3, s4, s5, s6, s7)) 6 = s6
getPieceOfLine (Line (s0, s1, s2, s3, s4, s5, s6, s7)) 7 = s7

setPieceOfLine :: Line -> Int -> Maybe Piece -> Line
setPieceOfLine (Line (s0, s1, s2, s3, s4, s5, s6, s7)) 0 p = Line (p, s1, s2, s3, s4, s5, s6, s7)
setPieceOfLine (Line (s0, s1, s2, s3, s4, s5, s6, s7)) 1 p = Line (s0, p, s2, s3, s4, s5, s6, s7)
setPieceOfLine (Line (s0, s1, s2, s3, s4, s5, s6, s7)) 2 p = Line (s0, s1, p, s3, s4, s5, s6, s7)
setPieceOfLine (Line (s0, s1, s2, s3, s4, s5, s6, s7)) 3 p = Line (s0, s1, s2, p, s4, s5, s6, s7)
setPieceOfLine (Line (s0, s1, s2, s3, s4, s5, s6, s7)) 4 p = Line (s0, s1, s2, s3, p, s5, s6, s7)
setPieceOfLine (Line (s0, s1, s2, s3, s4, s5, s6, s7)) 5 p = Line (s0, s1, s2, s3, s4, p, s6, s7)
setPieceOfLine (Line (s0, s1, s2, s3, s4, s5, s6, s7)) 6 p = Line (s0, s1, s2, s3, s4, s5, p, s7)
setPieceOfLine (Line (s0, s1, s2, s3, s4, s5, s6, s7)) 7 p = Line (s0, s1, s2, s3, s4, s5, s6, p)
	
getPieceOfBoard :: Board -> Square -> Maybe Piece
getPieceOfBoard (Board (l0, l1, l2, l3, l4, l5, l6, l7)) (0, n) = getPieceOfLine l0 n
getPieceOfBoard (Board (l0, l1, l2, l3, l4, l5, l6, l7)) (1, n) = getPieceOfLine l1 n
getPieceOfBoard (Board (l0, l1, l2, l3, l4, l5, l6, l7)) (2, n) = getPieceOfLine l2 n
getPieceOfBoard (Board (l0, l1, l2, l3, l4, l5, l6, l7)) (3, n) = getPieceOfLine l3 n
getPieceOfBoard (Board (l0, l1, l2, l3, l4, l5, l6, l7)) (4, n) = getPieceOfLine l4 n
getPieceOfBoard (Board (l0, l1, l2, l3, l4, l5, l6, l7)) (5, n) = getPieceOfLine l5 n
getPieceOfBoard (Board (l0, l1, l2, l3, l4, l5, l6, l7)) (6, n) = getPieceOfLine l6 n
getPieceOfBoard (Board (l0, l1, l2, l3, l4, l5, l6, l7)) (7, n) = getPieceOfLine l7 n

setPieceOfBoard :: Board -> Square -> Maybe Piece -> Board
setPieceOfBoard board (row, column) piece = setLineOfBoard board row newLine
	where	
		newLine = setPieceOfLine (getLineOfBoard board row) column piece
		setLineOfBoard (Board (l0, l1, l2, l3, l4, l5, l6, l7)) 0 p = Board (p, l1, l2, l3, l4, l5, l6, l7)
		setLineOfBoard (Board (l0, l1, l2, l3, l4, l5, l6, l7)) 1 p = Board (l0, p, l2, l3, l4, l5, l6, l7)
		setLineOfBoard (Board (l0, l1, l2, l3, l4, l5, l6, l7)) 2 p = Board (l0, l1, p, l3, l4, l5, l6, l7)
		setLineOfBoard (Board (l0, l1, l2, l3, l4, l5, l6, l7)) 3 p = Board (l0, l1, l2, p, l4, l5, l6, l7)
		setLineOfBoard (Board (l0, l1, l2, l3, l4, l5, l6, l7)) 4 p = Board (l0, l1, l2, l3, p, l5, l6, l7)
		setLineOfBoard (Board (l0, l1, l2, l3, l4, l5, l6, l7)) 5 p = Board (l0, l1, l2, l3, l4, p, l6, l7)
		setLineOfBoard (Board (l0, l1, l2, l3, l4, l5, l6, l7)) 6 p = Board (l0, l1, l2, l3, l4, l5, p, l7)
		setLineOfBoard (Board (l0, l1, l2, l3, l4, l5, l6, l7)) 7 p = Board (l0, l1, l2, l3, l4, l5, l6, p)
		getLineOfBoard (Board (l0, l1, l2, l3, l4, l5, l6, l7)) 0  = l0
		getLineOfBoard (Board (l0, l1, l2, l3, l4, l5, l6, l7)) 1  = l1
		getLineOfBoard (Board (l0, l1, l2, l3, l4, l5, l6, l7)) 2  = l2
		getLineOfBoard (Board (l0, l1, l2, l3, l4, l5, l6, l7)) 3  = l3
		getLineOfBoard (Board (l0, l1, l2, l3, l4, l5, l6, l7)) 4  = l4
		getLineOfBoard (Board (l0, l1, l2, l3, l4, l5, l6, l7)) 5  = l5
		getLineOfBoard (Board (l0, l1, l2, l3, l4, l5, l6, l7)) 6  = l6
		getLineOfBoard (Board (l0, l1, l2, l3, l4, l5, l6, l7)) 7  = l7

isInBoard :: Square -> Bool
isInBoard (row, column) = row >= 0 && column >= 0 && row < 8 && column < 8

isOccupied :: Board -> Square -> Bool
isOccupied board  = isJust . (getPieceOfBoard board)

canBeCapturedBy :: Board -> Square -> Color -> Bool
canBeCapturedBy b sq killerColor = case getPieceOfBoard b sq of
									Just (Piece color _) -> color == inverseColor killerColor
									otherwise -> False



pieceMovesGenerator :: Kind -> Square -> [[Square]]

pieceMovesGenerator Pawn (row, column) = undefined -- this function is not intended for pawns
				
pieceMovesGenerator King (row, column) = [[(row, column + 1)], [(row + 1, column + 1)], [(row + 1, column)], [(row + 1, column - 1)], 
	[(row, column - 1)], [(row - 1, column - 1)], [(row - 1, column)], [(row - 1, column + 1)]]

pieceMovesGenerator Knight (row, column) = [[(row + 1, column + 2)], [(row - 1, column + 2)], [(row + 2, column + 1)], [(row - 2, column + 1)],
	[(row + 1, column - 2)], [(row - 1, column - 2)], [(row + 2, column - 1)], [(row - 2, column - 1)]]
	
pieceMovesGenerator Bishop (row, column) = [ 
	map (\n -> (row + n, column + n)) [1..],
	map (\n -> (row - n, column + n)) [1..],
	map (\n -> (row - n, column - n)) [1..],
	map (\n -> (row + n, column - n)) [1..]]
	
pieceMovesGenerator Rook (row, column) = [ 
	map (\n -> (row + n, column)) [1..],
	map (\n -> (row - n, column)) [1..],
	map (\n -> (row, column + n)) [1..],
	map (\n -> (row, column - n)) [1..]]
	
pieceMovesGenerator Queen sq = pieceMovesGenerator Bishop sq ++ pieceMovesGenerator Rook sq

pieceMovesGeneratorWithMem :: Kind -> Square -> [[Square]]
pieceMovesGeneratorWithMem kind sq = allMovesGenerators Map.! (kind, sq)
	where
		allMovesGenerators = Map.fromList $ map (\key@(k, s) -> (key, pieceMovesGenerator k s)) allKindsAndSquares
		allKindsAndSquares = [(k, s) | k <- [King, Knight, Bishop, Rook, Queen], s <- allSquares] 


extractValidSquares :: Board -> Color -> [[Square]] -> [Square]
extractValidSquares b colorToMove movesLists = foldl' filterAndMerge [] movesLists
	where 
		filterAndMerge oldMoves squares = oldMoves ++ moves ++ captures
			where (moves, captures) = filterMovesAndCaptures ([],[]) squares
		filterMovesAndCaptures mc [] = mc
		filterMovesAndCaptures (moves, _) (sq:squares) = 
			if not $ isInBoard sq then (moves, [])
			else if not $ isOccupied b sq then filterMovesAndCaptures (sq:moves, []) squares
			else if canBeCapturedBy b sq colorToMove then (moves, [sq])
			else (moves, [])


getPawnMoves :: Position -> Square -> [Square]				
getPawnMoves position sq@(row, column) = map snd $ filter fst possibleMoves 
	where 
		ntm = nextToMove position 
		ep = enPassant position
		brd = board position
		isValidCaptureForPawn s = isInBoard s && ((canBeCapturedBy brd s ntm) || (Just s == ep))
		isValidMoveForPawn s = isInBoard s && (not $ isOccupied brd s)
		isValidLongMoveForPawn s@(r, _) = ((r == 6 && ntm == Black) || (r == 1 && ntm == White)) && isValidMoveForPawn s
		(nextSq, nextNextSq, leftCaptureSq, rightCaptureSq) = case ntm of
				White -> ((row + 1, column), (row + 2, column), (row + 1, column - 1), (row + 1, column + 1))
				Black -> ((row - 1, column), (row - 2, column), (row - 1, column + 1), (row - 1, column - 1))
		possibleMoves = [(isValidMoveForPawn nextSq, nextSq), (isValidMoveForPawn nextSq && isValidLongMoveForPawn nextNextSq, nextNextSq),
				(isValidCaptureForPawn leftCaptureSq, leftCaptureSq), (isValidCaptureForPawn rightCaptureSq, rightCaptureSq)]
			
					
		
getCastlings :: Position -> [Move]	
getCastlings position = resolveCastling possibleCastling
	where
		ntm = nextToMove position 
		brd = board position 
		possibleCastling = 
			case ntm of
				White -> whiteCastling position
				Black -> blackCastling position 
	 
		castlingSquares White KingCastling = ((0,4), [(0,5), (0,6)], (0,7))
		castlingSquares Black KingCastling = ((7,4), [(7,5), (7,6)], (7,7))
		castlingSquares White QueenCastling = ((0,4), [(0,3), (0,2), (0,1)], (0,0))
		castlingSquares Black QueenCastling = ((7,4), [(7,3), (7,2), (7,1)], (7,0))
		sqGoodForCastlingPath sq = not (isOccupied brd sq || isUnderAttackOf brd (inverseColor ntm) sq)
		sqGoodForKing sq = ((Just $ Piece ntm King) == getPieceOfBoard brd sq) && (not $ isUnderAttackOf brd (inverseColor ntm) sq)
		sqGoodForRook sq = ((Just $ Piece ntm Rook) == getPieceOfBoard brd sq)
		testCastling castling = (all sqGoodForCastlingPath path) && (sqGoodForKing kingSq) && (sqGoodForRook rookSq) 
								where (kingSq, path, rookSq) = castlingSquares ntm castling 
		resolveCastling c = case c of
							BothCastling -> resolveCastling KingCastling ++ resolveCastling QueenCastling
							NoCastling -> []
							QueenCastling -> if testCastling QueenCastling then [CastleToQueenSide] else []
							KingCastling -> if testCastling KingCastling then [CastleToKingSide] else [] 
	
			

getMoves :: Position -> Square -> [Move]	
getMoves position sq =
	let mbPiece = getPieceOfBoard (board position) sq in
	let	ntm = nextToMove position in
	let brd = board position in
	if (isNothing mbPiece) then []
	else		
		let piece = fromJust mbPiece in
		if ntm /= color piece then [] 
		else
		let	makeMove toSq = Move sq toSq piece Nothing in		
		let	makePawnMove toSq@(row,_) = 
				if row == 0 || row == 7 then [(Move sq toSq piece (Just Queen)), (Move sq toSq piece (Just Rook)), (Move sq toSq piece (Just Bishop)), (Move sq toSq piece (Just Knight))] 
										else [Move sq toSq piece Nothing] in
		case piece of 
			(Piece _ Pawn) -> concatMap makePawnMove $ getPawnMoves position sq
			(Piece _ King) ->(map makeMove $ extractValidSquares brd ntm $ pieceMovesGeneratorWithMem King sq) ++ (getCastlings position)
			(Piece _ kind) -> map makeMove $ extractValidSquares brd ntm $ pieceMovesGeneratorWithMem kind sq



getLegalMoves :: Position -> Square -> [Move]
getLegalMoves position sq = filter isLegalMove moves 
								where 
									moves = getMoves position sq
									isLegalMove move = not $ isCheck (board $ applyMove position move) (nextToMove position)
									
getAllMoves :: Position -> [Move]
getAllMoves p = concatMap (getMoves p) allSquares

getAllLegalMoves :: Position -> [Move]
getAllLegalMoves p = concatMap (getLegalMoves p) allSquares
		
applyMove :: Position -> Move -> Position
applyMove position (Move from to piece promotion) = Position newBoard2 ntm newWhiteCastling newBlackCastling newEnPassant halfMovesCount movesCount 
	where 
		ntm = inverseColor $ nextToMove position		
		movesCount = fullMoves position + if ntm == White then 1 else 0
		isPawnMove = Pawn == kind piece
		isCapturing = not $ isNothing $ getPieceOfBoard (board position) to
		halfMovesCount = if isPawnMove || isCapturing then 0 else 1 + halfMovesSinceAction position
		newPiece = if isNothing promotion then piece else piece { kind = fromJust promotion }
		newBoard1 = setPieceOfBoard (board position) from Nothing
		newBoard2 = setPieceOfBoard newBoard1 to (Just newPiece)
		newEnPassant = if isPawnMove && (abs (fst to - fst from) == 2) 
							then Just ((fst to + fst from) `div` 2, snd to) 
							else Nothing
		newWhiteCastling = case (from, whiteCastling position) of
								((0,4), _) -> NoCastling
								((0,0), BothCastling) -> KingCastling
								((0,0), QueenCastling) -> NoCastling
								((0,7), BothCastling) -> QueenCastling
								((0,7), QueenCastling) -> NoCastling
								(_, c) -> c
		newBlackCastling = case (from, blackCastling position) of
								((7,4), _) -> NoCastling
								((7,0), BothCastling) -> KingCastling
								((7,0), QueenCastling) -> NoCastling
								((7,7), BothCastling) -> QueenCastling
								((7,7), QueenCastling) -> NoCastling
								(_, c) -> c		

applyMove position castle = Position newBoard ntm newWhiteCastling newBlackCastling newEnPassant halfMovesCount movesCount 
	where 
		ntm = inverseColor $ nextToMove position		
		movesCount = fullMoves position + if ntm == White then 1 else 0
		halfMovesCount = 1 + halfMovesSinceAction position
		oldBoard = (board position)
		(oldKingSq, newKingSq, oldRookSq, newRootSq) = case (nextToMove position, castle) of
				(White, CastleToKingSide) -> ((0,4), (0,6), (0,7), (0,5))
				(White, CastleToQueenSide) -> ((0,4), (0,2), (0,0), (0,3))
				(Black, CastleToKingSide) -> ((7,4), (7,6), (7,7), (7,5))
				(Black, CastleToQueenSide) -> ((7,4), (7,2), (7,0), (7,3))	
		move from to b = setPieceOfBoard (setPieceOfBoard b to (getPieceOfBoard b from)) from Nothing
		newBoard = move oldKingSq newKingSq $ move oldRookSq newRootSq $ oldBoard		
		newWhiteCastling = if White == nextToMove position then NoCastling else whiteCastling position
		newBlackCastling = if Black == nextToMove position then NoCastling else blackCastling position
		newEnPassant = Nothing

								
		
isUnderAttackOf	:: Board -> Color -> Square -> Bool
isUnderAttackOf board enemyColor sq = any hitsTarget moves
		where 
			tmpBoard = setPieceOfBoard board sq $ Just $ Piece (inverseColor enemyColor) Pawn
			tmpPosition = emptyPosition { board = tmpBoard, nextToMove = enemyColor }
			moves = getAllMoves tmpPosition
			hitsTarget (Move _ to _ _) = to == sq
			hitsTarget _ = False
		
isCheckmate :: Board -> Color -> Bool
isCheckmate brd color = isCheck brd color && nowhereToMove
		where
			position = Position brd color NoCastling NoCastling Nothing 0 1
			nowhereToMove = null $ getAllLegalMoves position
		
isCheck :: Board -> Color -> Bool
isCheck brd color = 
	case squereOfKing of
		Just sq -> isUnderAttackOf brd (inverseColor color) sq
		Nothing -> False
	where 
		squereOfKing = find isKing allSquares
		isKing s = Just (Piece color King) == getPieceOfBoard brd s

