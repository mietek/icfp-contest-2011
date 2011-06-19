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
    i <- begin program
    loop i i
  where
    loop i0 i
      | finished i = loop i0 i0
      | otherwise = do
          i' <- next i
          liftIO $ do
            putOurMove (fromJust (current i))
            ignoreTheirMove
          loop i0 i'

putOurMove :: Move -> IO ()
putOurMove (ApplyL card slotNumber) = do
  putStrLn (show (fromEnum LeftApplication))
  putStrLn (show card)
  putStrLn (show slotNumber)
putOurMove (ApplyR slotNumber card) = do
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
