module Main where

import Control.Monad (when)
import Control.Monad.CC
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List (foldl')
import Data.Maybe (fromJust)
import System.Environment (getArgs)
import System.IO (BufferMode (..), hSetBuffering, stdin, stdout)

import Common


--------------------------------------------------------------------------------

-- 1 zero
-- for [0 .. 255]
--     for [0 .. 9999]
--         0 zero
--         succ 0
--         get 0
--         dec 0
--     succ 1

--------------------------------------------------------------------------------

data Move = ApplyL Card SlotNumber | ApplyR SlotNumber Card
  deriving Show

data Program = Move Move | Concat [Program] | Replicate Int [Program]
  deriving Show

mainProgram :: Program
mainProgram =
  Concat [
    Move (ApplyR 1 Zero),
    Replicate 256 [
      Replicate 10000 [
        Move (ApplyR 0 Zero),
        Move (ApplyL Succ 0),
        Move (ApplyL Get 0),
        Move (ApplyL Dec 0)
      ],
      Move (ApplyL Succ 1)
    ]
  ]

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

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  player <- fmap (toEnum . read . head) getArgs
  play player

play :: Player -> IO ()
play player = do
  when (player == Them) ignoreTheirMove
  runCCT $ do
    i <- begin mainProgram
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
