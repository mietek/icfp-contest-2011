module IgnorantCommon where

import Control.Monad.CC (runCCT)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (fromJust)

import Common


--------------------------------------------------------------------------------

play :: Program -> Player -> IO ()
play program player = do
  when (player == Them) ignoreTheirMove
  runCCT $ do
    start <- newIterator program
    loop start start
  where
    loop start iterator
      | isIteratorDone iterator = loop start start
      | otherwise = do
          iterator' <- nextIterator iterator
          liftIO $ do
            putOurMove (fromJust (iteratorCurrent iterator))
            ignoreTheirMove
          loop start iterator'

putOurMove :: Move -> IO ()
putOurMove move =
  case move of
    ApplyL card slotNumber -> do
      putStrLn (show (fromEnum LeftApplication))
      putStrLn (show card)
      putStrLn (show slotNumber)
    ApplyR slotNumber card -> do
      putStrLn (show (fromEnum RightApplication))
      putStrLn (show slotNumber)
      putStrLn (show card)

ignoreTheirMove :: IO ()
ignoreTheirMove = do
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

--------------------------------------------------------------------------------
