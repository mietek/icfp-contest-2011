module Main where

import System.Environment (getArgs)
import System.IO (BufferMode (..), hSetBuffering, stdin, stdout)

import Common


main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  player <- fmap (toEnum . read . head) getArgs
  case player of
    Us -> putOurMove
    Them -> getTheirMove

putOurMove :: IO ()
putOurMove = do
  putStrLn "1"
  putStrLn "I"
  putStrLn "0"
  getTheirMove

getTheirMove :: IO ()
getTheirMove = do
  move <- fmap (toEnum . read) getLine
  case move of
    LeftApplication -> do
      card <- getLine
      slotNumber <- getLine
      return ()
    RightApplication -> do
      slotNumber <- getLine
      card <- getLine
      return ()
  putOurMove
