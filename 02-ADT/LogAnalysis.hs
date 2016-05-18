{-# OPTIONS_GHC -Wall #-}
-- module LogAnalysis where
import Log
import System.IO

parseMessage :: String -> LogMessage
parseMessage msg@('I':xs)
    | (length $ words xs) > 1 = 
                            let severity = read (words xs !! 0) :: Int
                                message = unwords $ drop 1 $ words xs
                            in LogMessage Info severity message
    | otherwise = Unknown msg
parseMessage msg@('W':xs)
    | (length $ words xs) > 1 = 
                            let severity = read (words xs !! 0) :: Int
                                message = unwords $ drop 1 $ words xs
                            in LogMessage Warning severity message
    | otherwise = Unknown msg
parseMessage msg@('E':xs)
    | (length $ words xs) > 2 = 
                            let severity = read (words xs !! 0) :: Int
				code = read (words xs !! 1) :: Int
                                message = unwords $ drop 2 $ words xs
                            in LogMessage (Error code) severity message
    | otherwise = Unknown msg
parseMessage x = Unknown x

parse :: String -> [LogMessage]
parse content = map parseMessage $ lines content

