{-# LANGUAGE TypeSynonymInstances, FlexibleInstances  #-}
 
module Main (main) where

import System.IO
import Data.List
import Data.Char
import Data.Maybe
import System.Random

import Chess
import FEN
import UCI

main = uci
	--putStrLn $ renderFEN $ initialPosition
	--step (mkStdGen 1) initialPosition


step :: StdGen -> Position -> IO ()
step g p = do
	putStrLn "--------------------------------------------"
	putStrLn $ show p
	let moves = getAllLegalMoves p
	let (index, newG) = random g 
	let isEnd = (null moves) || (halfMovesSinceAction p > 50)
	if isEnd then return () 
				else do
					let move = (moves !! (index `mod` (length moves)))
					putStrLn $ show move 
					step newG $ applyMove p move

			
								
	
