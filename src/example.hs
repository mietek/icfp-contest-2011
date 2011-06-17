module Main where

import System.Environment (getArgs)
import System.IO (BufferMode (..), hSetBuffering, stdin, stdout)

main :: IO ()
main = do
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering
    playerId <- fmap head getArgs
    if playerId == "1"
        then getOpponentsMove
        else putMove

putMove :: IO ()
putMove = do
    putStrLn "1"
    putStrLn "I"
    putStrLn "0"
    getOpponentsMove

getOpponentsMove :: IO ()
getOpponentsMove = do
    moveType <- getLine
    (card, slot) <- case moveType of
        "1" -> do
            card <- getLine
            slot <- getLine
            return (card, slot)
        "2" -> do
            slot <- getLine
            card <- getLine
            return (card, slot)
    putMove
