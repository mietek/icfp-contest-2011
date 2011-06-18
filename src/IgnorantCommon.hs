module IgnorantCommon where

import Control.Monad (when)
import Control.Monad.CC
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List (foldl')
import Data.Maybe (fromJust)

import Common


--------------------------------------------------------------------------------

data Program = Move Move | Concat [Program] | Replicate Int [Program]
  deriving Show

foldProgram :: (a -> Move -> a) -> a -> Program -> a
foldProgram f a (Move m) = f a m
foldProgram f a (Concat ps) = foldl' (foldProgram f) a (reverse ps)
foldProgram _ a (Replicate 0 _) = a
foldProgram f a (Replicate n ps) =
  let a' = foldl' (foldProgram f) a (reverse ps)
  in foldProgram f a' (Replicate (n - 1) ps)

forProgram :: Monad m => Program -> (Move -> m ()) -> m ()
forProgram program f = foldProgram (\a m -> f m >> a) (return ()) program

--------------------------------------------------------------------------------

data Iterator m = Done | Current Move (m (Iterator m))

begin :: MonadDelimitedCont p s m => Program -> m (Iterator m)
begin program =
  reset $ \d ->
    forProgram program (\m ->
      shift d (\k -> return (Current m (k (return ()))))) >> return Done

current :: Iterator m -> Maybe Move
current Done = Nothing
current (Current m _) = Just m

next :: Monad m => Iterator m -> m (Iterator m)
next Done = return Done
next (Current _ i) = i

finished :: Iterator m -> Bool
finished Done = True
finished _ = False

--------------------------------------------------------------------------------

play :: Program -> Player -> IO ()
play program player = do
  when (player == Them) ignoreTheirMove
  runCCT $ do
    i <- begin program
    loop i
  where
    loop i
      | finished i = return ()
      | otherwise = do
          i' <- next i
          liftIO $ do
            putOurMove (fromJust (current i))
            ignoreTheirMove
          loop i'

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
