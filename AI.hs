module AI where

import Data.List
import Data.Char
import Data.Maybe
import System.IO
import System.Random

import Control.Monad
import Control.Monad.State
import Control.Parallel
import Control.Parallel.Strategies

import Chess

getBestMove :: Position -> IO Move
getBestMove pos = do
                    rnd <- (randomIO :: IO Int)
                    let moves = getAllLegalMoves pos
                    let evaluateMove move =  evaluateDeep 3 $ applyMove pos move
                    let movesWithValue = zip moves $ parMap rseq evaluateMove moves
                    let criterion = if nextToMove pos == White then maximumBy else minimumBy
                    let move = fst $ criterion (\m1 m2 -> compare (snd m1) (snd m2)) movesWithValue
                    return move

evaluateDeep :: Int -> Position -> Int
evaluateDeep depth position = score (alpha_beta position depth (-infinityScore) infinityScore)

------- Alpha-beta pruning
data Variation = Variation { moves :: [Move], score :: Int }
alpha_beta :: Position -- Position to be evaluated
           -> Int -- Search this deep
           -> Int -- Minimum score maximising player (White) is assured
           -> Int -- Maximum score minimizing player (Black) is assured
           -> Variation  --  Principal variation
alpha_beta position 0 alpha beta = Variation {score = evaluate position, moves = []}
alpha_beta position depth alpha beta = 
        let allMoves = getAllLegalMoves position
            traverse :: [Move] -> Color -> State Variation () 
            traverse [] _ = do return ()
            traverse (move:restOfMoves) color = do 
                                    bestVar <- get
                                    let newPosition = applyMove position move
                                    let (newVar, cmp) = case color of 
                                                        White -> (alpha_beta newPosition (depth - 1) (score bestVar) beta, (>))
                                                        Black -> (alpha_beta newPosition (depth - 1) alpha (score bestVar), (<))
                                    if cmp (score newVar) (score bestVar) 
                                        then do 
                                            put (newVar { moves = move : (moves newVar) }) 
                                            let pruneThisBranch = case color of 
                                                                    White -> (score newVar) >= beta 
                                                                    Black -> (score newVar) <= alpha
                                            if pruneThisBranch 
                                                then do return ()
                                                else traverse restOfMoves color
                                        else traverse restOfMoves color
            in
            if depth == 0 || null allMoves then Variation { score = evaluate position, moves = []} 
            else case nextToMove position of
                White -> execState (traverse allMoves White) Variation { score = alpha, moves = [] }
                Black -> execState (traverse allMoves Black) Variation { score = alpha, moves = [] } 


-------
getAllPiecesOfBoard :: Board -> Color -> [Piece]
getAllPiecesOfBoard brd colorToGet = filter (\p -> colorToGet == color p) $ mapMaybe (getPieceOfBoard  brd) allSquares




-- evaluates position value for White
pawnScore = 100
infinityScore = 10000 * pawnScore

evaluate :: Position -> Int
evaluate p = (evaluateSide p White) - (evaluateSide p Black)

evaluateSide :: Position -> Color -> Int
evaluateSide p color = (evaluateMaterial p color) + (evaluateCheckmate p color) + (evaluateSpace p color)

evaluateSpace :: Position -> Color -> Int
evaluateSpace pos color = length $ getAllLegalMoves pos { nextToMove = color }

evaluateCheckmate :: Position -> Color -> Int
evaluateCheckmate p color = if isCheckmate brd color then -infinityScore 
                            else if isCheck brd color then -(pawnScore `div` 10) else 0
            where brd = board p

evaluateMaterial :: Position -> Color -> Int
evaluateMaterial p color = sum $ map evaluatePiece $ getAllPiecesOfBoard (board p) color
            where 
                evaluatePiece (Piece _ Pawn) = pawnScore
                evaluatePiece (Piece _  Bishop) = 3 * pawnScore
                evaluatePiece (Piece _  Knight) = 3 * pawnScore
                evaluatePiece (Piece _  Rook) = 5 * pawnScore
                evaluatePiece (Piece _  Queen) = 9 * pawnScore
                evaluatePiece _ = 0
