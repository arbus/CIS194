module LogAnalysis where

import qualified Text.ParserCombinators.Parsec as P
import Log

---------- Exercise 1 -----------

parseError :: P.Parser MessageType
parseError = do
	P.char 'E'
	P.skipMany1 P.space
	x <- P.many P.digit
	return $ Error (read x)

parseInfo :: P.Parser MessageType
parseInfo = do
	P.char 'I'
	return Info

parseWarning :: P.Parser MessageType
parseWarning = do
	P.char 'W'
	return Warning

parseMessageType :: P.Parser MessageType
parseMessageType = parseError P.<|> parseWarning P.<|> parseInfo

parseTimestamp :: P.Parser TimeStamp
parseTimestamp = do
	P.skipMany P.space
	x <- P.many P.digit
	return $ read x

parseLogMessage :: P.Parser LogMessage
parseLogMessage = do
	ty <- parseMessageType
	ts <- parseTimestamp
	P.skipMany P.space
	msg <- P.many P.anyChar
	return $ LogMessage ty ts msg

parseMessage :: String -> LogMessage
parseMessage x = case P.parse parseLogMessage "" x of 
	Left  _ -> Unknown x
	Right y -> y

parse :: String -> [LogMessage]
parse = (map parseMessage) . lines

---------- Exercise 2 -----------

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) x = x
insert xs@(LogMessage _ x _) ys@(Node a as@(LogMessage _ y _) b)
	| x > y     = Node a as (insert xs b)
	| x < y     = Node (insert xs a) as b
	| otherwise = ys
insert x Leaf = Node Leaf x Leaf

---------- Exercise 3 -----------

build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf

---------- Exercise 4 -----------

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf         = []
inOrder (Node a x b) = (inOrder a) ++ [x] ++ (inOrder b)

---------- Exercise 5 -----------

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = (map (\(LogMessage _ _ s) -> s)) . (filter errorHigherThan50) . reverse . inOrder . build
	where
	errorHigherThan50 :: LogMessage -> Bool
	errorHigherThan50 (LogMessage (Error e) _ _) = e > 50
	errorHigherThan50 _                          = False