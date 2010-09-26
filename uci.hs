module UCI where


import Data.List
import Data.IORef
import System.Exit
import Data.Char
import Data.Maybe
import System.IO
import System.Random

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
			| RspOption String 
			

---------------- show ------------------
instance Show Response where
		show RspUciOk = "uciok"
		show RspReadyOk = "readyok"
		show (RspInfo info) = "info " ++ info
		show (RspId name value) = "id " ++ name ++ " " ++ value
		show (RspBestMove move) = "bestmove " ++ renderShortMove move
		show (RspOption text) = "option " ++ text
		
		
renderShortMove :: Move -> String
renderShortMove CastleToKingSide = "O-O-O"
renderShortMove CastleToQueenSide = "O-O"
renderShortMove (Move from to piece promotion) = showSquare from ++ showSquare to ++ (showPromotion promotion)
								where 
									showSquare  (row, column) = (['a' .. 'h'] !! column) : (show $ 1 + row)									
									showPromotion (Just Queen) = "q"
									showPromotion (Just Knight) = "n"
									showPromotion (Just Rook) = "r"
									showPromotion (Just Bishop) = "b"
									showPromotion _ = ""
	
------------------ parsers --------------
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
				many $ char ' '
				mbTimeout <- optionMaybe (string "movetime" >> (many $ char ' ') >> p_int)
				return $ case mbTimeout of
							Nothing -> CmdGo Infinity
							Just timeout -> CmdGo $ MovetimeMsc timeout
					
	
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


		
					
getBestMove :: Position -> IO Move
getBestMove pos = do
					rnd <- (randomIO :: IO Int)
					let moves = getAllLegalMoves pos
					let index = rnd `mod` (length moves)
					let move = (moves !! index)
					return move
					
				
uci :: IO ()
uci = do
	hSetBuffering stdout NoBuffering
	line <- getLine
	appendFile "sillyLog.log" ("GUI>" ++ line ++ "\n")
	lastPosition <- newIORef initialPosition
	case parseCommand line of
		Nothing -> return ()
		Just cmd -> do 
						responses <- getResponse cmd
						let output = intercalate "\n" $ map show $ responses
						appendFile "sillyLog.log" ("Engine>" ++ output ++ "\n")
						putStrLn output
					    	where
								getResponse CmdUci = return [(RspId "name" "sillyChess")
										, (RspId "author" "EZ")
										--, (RspOption "name NalimovPath type string default <empty>")
										--, (RspOption "name NalimovCache type spin default 1 min 1 max 64")
										--, (RspOption "name ClearHash type button")
										--, (RspOption "name UseNalimov type check default false")										
										--, (RspOption "name Hash type spin default 1 min 1 max 128")
										--, (RspOption "name OwnBook type check default true")
										, RspUciOk]
								getResponse	CmdIsReady = return [RspReadyOk]
								getResponse	CmdUciNewGame = return []
								getResponse	CmdQuit = exitWith ExitSuccess
								getResponse	CmdStop = return []
								getResponse (CmdPosition pos moves)  = do
										let finalPosition = foldl' applyMove pos moves
										writeIORef lastPosition finalPosition
										return []
								getResponse (CmdGo so) = do
										position <- readIORef lastPosition
										move <- getBestMove position
										return $ [RspInfo ("currmove " ++ renderShortMove move), RspBestMove move]
					   
	uci

	

	
	