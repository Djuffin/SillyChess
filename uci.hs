module UCI where

import Data.List
import Data.Char
import Data.Maybe
import System.IO

import Text.ParserCombinators.Parsec
import Control.Monad

import FEN
import Chess

data SearchOption = MovetimeMsc Int | Infinity
					deriving (Show)
					
data Command = CmdUci 
			| CmdIsReady 
			| CmdUciNewGame 
			| CmdPosition Position [Move] 
			| CmdGo SearchOption
			| CmdStop
			| CmdQuit
			deriving (Show)

data Response = RspId String String
			| RspUciOk
			| RspReadyOk
			| RspBestMove Move
			| RspInfo String
			deriving (Show)

	
------------------ parsers -----------	---
p_cmd_uci = do
				string "uci"
				return CmdUci

p_cmd_isready = do
				string "isready"
				return CmdIsReady			
				
p_cmd_ucinewgame = do
				string "ucinewgame"
				return CmdUciNewGame				
				
p_cmd_stop = do
				string "stop"
				return CmdStop
				
p_cmd_quit = do
				string "quit"
				return CmdQuit
				
p_cmd_go = do
				string "go"
				return $ CmdGo Infinity
	
p_cmd_position :: CharParser ()	Command
p_cmd_position = do
				string "position"
				many1 $ char ' '
				posType <- (string "fen" <|> string "startpos")
				many $ char ' '
				pos <- if posType == "fen" then p_position else return initialPosition
				many $ char ' '
				moves <- option [] (string "moves" >> (many $ p_move pos))		
				return $ CmdPosition pos moves

p_move :: Position -> CharParser ()	Move
p_move pos = do	
			optionMaybe $ char ' '
			from <- liftM fromJust p_squere
			to <- liftM fromJust p_squere
			promotionCh <- optionMaybe $ oneOf ['q', 'r', 'b', 'n' ]
			let promotion = lookup (fromMaybe 'X' promotionCh) [('q', Queen), ('r', Rook), ('b', Bishop), ('n', Knight)] 
			let mbPiece = getPieceOfBoard (board pos) from
			case mbPiece of
				Just piece -> return $ Move from to piece promotion 
				Nothing -> fail "illegal move"
			

p_cmd = (try p_cmd_ucinewgame) <|> p_cmd_uci <|> p_cmd_isready <|> p_cmd_stop <|> p_cmd_quit <|> p_cmd_go <|> p_cmd_position	

parseCommand :: String -> Maybe Command
parseCommand line = case parse p_cmd "" line of
				Left _ -> Nothing
				Right cmd -> Just cmd

uci :: IO ()
uci = do
	line <- getLine
	case parseCommand line of
		Nothing -> putStrLn "no command"
		Just cmd -> putStrLn $ show cmd
	uci
	
	