module Chess where

import Data.List
import Data.Char
import Data.Maybe

data Kind = King | Queen | Rook | Bishop | Knight | Pawn
                 deriving (Eq)
				 
data Color = White | Black
                 deriving (Eq)
				 
data Piece = Piece { color :: Color, kind :: Kind }
				deriving (Eq)
				
type Square	= (Int, Int) -- row, column (from zero)

data Line = Line [Maybe Piece]				
				deriving (Eq)

data Board = Board [Line] -- rows from 1 to 8
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

initialBoard = Board [
					(Line [Just (Piece White Rook), Just (Piece White Knight), Just (Piece White Bishop), Just (Piece White Queen), Just (Piece White King), Just (Piece White Bishop), Just (Piece White Knight), Just (Piece White Rook)]),
					(Line [Just (Piece White Pawn), Just (Piece White Pawn), Just (Piece White Pawn), Just (Piece White Pawn), Just (Piece White Pawn), Just (Piece White Pawn), Just (Piece White Pawn), Just (Piece White Pawn)]),
					(Line [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]),
					(Line [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]),
					(Line [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]),
					(Line [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]),
					(Line [Just (Piece Black Pawn), Just (Piece Black Pawn), Just (Piece Black Pawn), Just (Piece Black Pawn), Just (Piece Black Pawn), Just (Piece Black Pawn), Just (Piece Black Pawn), Just (Piece Black Pawn)]),
					(Line [Just (Piece Black Rook), Just (Piece Black Knight), Just (Piece Black Bishop), Just (Piece Black Queen), Just (Piece Black King), Just (Piece Black Bishop), Just (Piece Black Knight), Just (Piece Black Rook)])
				]

initialPosition = Position initialBoard White BothCastling BothCastling Nothing 0 1
allSquares = [(i,j) | i <- [0,7], j <- [0..7]]

data Move = Move {
				from :: Square,
				to :: Square,
				piece :: Piece
			}
			| CastleToKingSide
			| CastleToQueenSide

			
---------------- functions ----------------

inverseColor White = Black
inverseColor Black = White

getPieceOfLine :: Line -> Int -> Maybe Piece
getPieceOfLine (Line pieces) n = pieces !! n

setPieceOfLine :: Line -> Int -> Maybe Piece -> Line
setPieceOfLine (Line pieces) index piece = Line (prefix ++ [piece] ++ postfix)
	where	
		prefix = take (index - 1) pieces
		postfix = drop index pieces

getPieceOfBoard :: Board -> Square -> Maybe Piece
getPieceOfBoard (Board rows) (row, column) =  getPieceOfLine (rows !! row) column

setPieceOfBoard :: Board -> Square -> Maybe Piece -> Board
setPieceOfBoard (Board rows) (row, column) piece = Board (prefix ++ [setPieceOfLine line column piece] ++ postfix)
	where	
		prefix = take (row - 1) rows
		postfix = drop row rows
		line = rows !! row

isInBoard :: Square -> Bool
isInBoard (row, column) = and [row > 0, column > 0, row < 8, column < 8]

isOccupied :: Board -> Square -> Bool
isOccupied board  = isJust . (getPieceOfBoard board)

canBeCapturedBy :: Board -> Square -> Color -> Bool
canBeCapturedBy b sq killerColor = case getPieceOfBoard b sq of
									Just victimColor -> True
									otherwise -> False
								where victimColor = inverseColor killerColor
								


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


filterMoves :: Board -> Color -> [[Square]] -> [Square]
filterMoves b colorToMove movesLists = moves ++ captures
	where (moves, captures) = filterMovesAndCaptures b colorToMove movesLists

filterMovesAndCaptures :: Board -> Color -> [[Square]] -> ([Square], [Square])
filterMovesAndCaptures b colorToMove movesLists = foldl filterAndMerge ([],[]) movesLists
	where 
		filterAndMerge (moves1, captures1) squares = (moves1 ++ moves2, captures1 ++ captures2)
			where (moves2, captures2) = filterMovesAndCaptures squares
		filterMovesAndCaptures squares = (moves, captures)
			where 
				(moves, rest) = span isMoveToEmpty squares
				(captures, _) = span isCapture rest
				isMoveToEmpty s = isInBoard s && (not $ isOccupied b s)
				isCapture s = isInBoard s && (canBeCapturedBy b s colorToMove)

getPawnMoves :: Position -> Square -> [Square]				
getPawnMoves position sq@(row, column) = map snd $ filter fst possibleMoves 
	where 
		ntm = nextToMove position 
		ep = enPassant position
		brd = board position
		isValidCaptureForPawn s = isInBoard s && ((canBeCapturedBy brd s ntm) || (Just s == ep))
		isValidMoveForPawn s = not $ isOccupied brd s
		nextSq = if ntm == White then (row + 1, column) else (row - 1, column)
		nextNextSq = if ntm == White then (row + 2, column) else (row - 2, column)
		leftCaptureSq = if ntm == White then (row + 1, column - 1) else (row - 1, column + 1)
		rightCaptureSq = if ntm == White then (row + 1, column + 1) else (row - 1, column - 1)
		possibleMoves = [(isValidMoveForPawn nextSq, nextSq), (isValidMoveForPawn nextSq && isValidMoveForPawn nextNextSq, nextNextSq),
				(isValidCaptureForPawn leftCaptureSq, leftCaptureSq), (isValidCaptureForPawn rightCaptureSq, rightCaptureSq)]
			
					
		
getCastlings :: Position -> [Move]	
getCastlings position = 
	let	ntm = nextToMove position in
	let brd = board position in
	let possibleCastling = 
			case ntm of
				White -> whiteCastling position
				Black -> blackCastling position in
	let  
			castlingSquares White KingCastling = [(0,4), (0,5), (0,6)]
			castlingSquares Black KingCastling = [(7,4), (7,5), (7,6)]
			castlingSquares White QueenCastling = [(0,4), (0,3), (0,2)]
			castlingSquares Black QueenCastling = [(7,4), (7,3), (7,2)] in	
	let testCastling castling = all (not . isUnderAttackOf brd (inverseColor ntm)) $ castlingSquares ntm castling in
	let resolveCastling c = case c of
							BothCastling -> resolveCastling KingCastling ++ resolveCastling QueenCastling
							NoCastling -> []
							QueenCastling -> if testCastling QueenCastling then [CastleToQueenSide] else []
							KingCastling -> if testCastling KingCastling then [CastleToKingSide] else [] in
	resolveCastling possibleCastling
			

getMoves :: Position -> Square -> [Move]	
getMoves position sq =
	let mbPiece = getPieceOfBoard (board position) sq in
	let	ntm = nextToMove position in
	let brd = board position in
	if (isNothing mbPiece) then []
	else
		let piece = fromJust mbPiece in
		let	makeMove toSq = Move sq toSq piece in		
		case piece of 
			(Piece nextToMove Pawn) -> map makeMove $ getPawnMoves position sq
			(Piece nextToMove King) ->(map makeMove $ filterMoves brd ntm $ pieceMovesGenerator King sq) ++ (getCastlings position)
			(Piece nextToMove kind) -> map makeMove $ filterMoves brd ntm $ pieceMovesGenerator kind sq


getLegalMoves :: Position -> Square -> [Move]
getLegalMoves position sq = filter isLegalMove moves 
								where 
									moves = getMoves position sq
									isLegalMove move = not $ isCheck (board $ applyMove position move) (nextToMove position)
		
applyMove :: Position -> Move -> Position
applyMove = undefined
		
isUnderAttackOf	:: Board -> Color -> Square -> Bool
isUnderAttackOf board color sq = undefined
		
isCheck :: Board -> Color -> Bool
isCheck brd color = 
	case squereOfKing of
		Just sq -> isUnderAttackOf brd (inverseColor color) sq
		Nothing -> False
	where 
		squereOfKing = find isKing allSquares
		isKing s = Just (Piece color King) == getPieceOfBoard brd s

