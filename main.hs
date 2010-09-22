{-# LANGUAGE TypeSynonymInstances, FlexibleInstances  #-}
 
module Main (main) where

import System.IO
import Data.List
import Data.Char
import Data.Maybe

import Chess
import FEN

main = do
	--putStrLn $ renderFEN $ initialPosition
	let moves = getAllLegalMoves initialPosition
	let strMoves = intersperse " " $ map show moves
	putStrLn $ concat strMoves


