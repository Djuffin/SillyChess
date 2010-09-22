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

emptyBoard = Board $ replicate 8 $ Line (replicate 8 Nothing)
				
initialPosition = Position initialBoard White BothCastling BothCastling Nothing 0 1
emptyPosition = Position emptyBoard White NoCastling NoCastling Nothing 0 1

allSquares = [(i,j) | i <- [0,7], j <- [0..7]]

data Move = Move {
				from :: Square,
				to :: Square,
				piece :: Piece,
				promotion :: Maybe Kind
			}
			| CastleToKingSide
			| CastleToQueenSide

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
	show (Line pieces) = concat $ map showPiece pieces
					where 
						showPiece Nothing = "."
						showPiece (Just p) = show p
					

instance Show Board where
	show (Board lines) = intercalate "\n" $ map show $ reverse lines
	
instance Show Position where
	show p = (show $ board p) ++ "\n " ++ (show $ nextToMove p) ++ " " ++ (show $ whiteCastling p)  ++ " " ++ (show $ blackCastling p) ++ 
				" " ++ (showSquare $ enPassant p) ++ " " ++ (show $ halfMovesSinceAction p) ++ " " ++ (show $ fullMoves p)
					where 
						showSquare Nothing = "-"
						showSquare (Just (row, column)) = (['a' .. 'h'] !! column) : (show $ 1 + column)			
			
---------------- functions ----------------

inverseColor White = Black
inverseColor Black = White

replaceByIndex :: [a] -> Int -> (a -> a) -> [a]		
replaceByIndex [] _ _ = []
replaceByIndex (x:xs) 0 f = (f x):xs
replaceByIndex (x:xs) n f = x : replaceByIndex xs (n - 1) f

getPieceOfLine :: Line -> Int -> Maybe Piece
getPieceOfLine (Line pieces) n = pieces !! n

setPieceOfLine :: Line -> Int -> Maybe Piece -> Line
setPieceOfLine (Line pieces) index piece = Line $ replaceByIndex pieces index (\_ -> piece)
	
getPieceOfBoard :: Board -> Square -> Maybe Piece
getPieceOfBoard (Board rows) (row, column) =  getPieceOfLine (rows !! row) column

setPieceOfBoard :: Board -> Square -> Maybe Piece -> Board
setPieceOfBoard (Board rows) (row, column) piece = Board $ replaceByIndex rows row replace
	where	
		replace line = setPieceOfLine line column piece 

isInBoard :: Square -> Bool
isInBoard (row, column) = and [row >= 0, column >= 0, row < 8, column < 8]

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
		(nextSq, nextNextSq, leftCaptureSq, rightCaptureSq) = case ntm of
				White -> ((row + 1, column), (row + 2, column), (row + 1, column - 1), (row + 1, column + 1))
				Black -> ((row - 1, column), (row - 2, column), (row - 1, column + 1), (row - 1, column - 1))
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
		if ntm /= color piece then [] 
		else
		let	makeMove toSq = Move sq toSq piece Nothing in		
		let	makePawnMove toSq@(row,_) = 
				if row == 0 || row == 7 then [(Move sq toSq piece (Just Queen)), (Move sq toSq piece (Just Rook)), (Move sq toSq piece (Just Bishop)), (Move sq toSq piece (Just Knight))] 
										else [Move sq toSq piece Nothing] in
		case piece of 
			(Piece _ Pawn) -> concatMap makePawnMove $ getPawnMoves position sq
			(Piece _ King) ->(map makeMove $ filterMoves brd ntm $ pieceMovesGenerator King sq) ++ (getCastlings position)
			(Piece _ kind) -> map makeMove $ filterMoves brd ntm $ pieceMovesGenerator kind sq



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
		
isCheck :: Board -> Color -> Bool
isCheck brd color = 
	case squereOfKing of
		Just sq -> isUnderAttackOf brd (inverseColor color) sq
		Nothing -> False
	where 
		squereOfKing = find isKing allSquares
		isKing s = Just (Piece color King) == getPieceOfBoard brd s

