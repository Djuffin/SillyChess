{-# LANGUAGE TypeSynonymInstances, OverlappingInstances #-}
module FEN_Tests  where

import Control.Monad
import Test.QuickCheck
import Data.Maybe
import Data.Char
import Data.List
import System.Random

import Chess
import FEN
import Chess_Tests

prop_renderAndRead :: Position -> Bool
prop_renderAndRead p = p == (fromJust $ readFEN $ renderFEN p)

instance Show Position where
	show p = renderFEN p

quickCheckArgs = Args (Just (mkStdGen 1, 1)) 300 1 300
	  
main = do 
	quickCheckWith quickCheckArgs  prop_renderAndRead 
