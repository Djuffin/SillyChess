module Cache (Map, emptyCache, put, tryGet) where

import Data.List
import Data.Char
import Data.Maybe
import System.IO
import System.Random

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq


data CacheMap k a = CacheMap { 
					maxSize :: Int, 
					map :: Map k a,
					queue :: Seq k
				}	

emptyCache :: Ord k => Int -> CacheMap k a
emptyCache maxSize = CacheMap maxSize Map.empty Seq.empty

put :: Ord k => CacheMap k a -> k -> a -> CacheMap k a
put (CacheMap maxSize map queue) key value = removeOverweight (CacheMap maxSize (Map.insert key value map) (queue Seq.|> key))
	where 
		removeOverweight cache@(CacheMap size map queue) = 
				let size = Map.size map in
				if maxSize >= size then cache
				else 
					let keyToDelete = Seq.index queue 0 in
					let newQueue = Seq.drop 1 queue in
					let newMap = Map.delete keyToDelete map in
					removeOverweight (CacheMap maxSize newMap newQueue)

tryGet :: Ord k => CacheMap k a -> k -> Maybe a 
tryGet (CacheMap maxSize map queue) key = Map.lookup key map

instance (Show k, Show a) => Show (CacheMap k a) where 
	show (CacheMap maxSize map queue) = show map 