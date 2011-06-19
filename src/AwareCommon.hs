module AwareCommon where

import Control.Monad (when)
import Control.Monad.CC (runCCT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.ST (ST, stToIO)
import Data.Array.ST (STArray, newArray, readArray, writeArray)
import Data.Maybe (fromJust, fromMaybe)
import GHC.Prim (RealWorld)

import Common


--------------------------------------------------------------------------------

type Game s = STArray s (Player, SlotNumber) Slot

newGame :: ST s (Game s)
newGame = newArray ((Us, 0), (Them, 255)) newSlot

readSlot :: Game s -> Player -> SlotNumber -> ST s Slot
readSlot game player slotNumber = readArray game (player, slotNumber)

writeSlot :: Game s -> Player -> SlotNumber -> Slot -> ST s ()
writeSlot game player slotNumber slot = writeArray game (player, slotNumber) slot

readSlotField :: Game s -> Player -> SlotNumber -> ST s Value
readSlotField game player slotNumber = do
  slot <- readSlot game player slotNumber
  return (slotField slot)

writeSlotField :: Game s -> Player -> SlotNumber -> Value -> ST s ()
writeSlotField game player slotNumber value = do
  slot <- readSlot game player slotNumber
  writeSlot game player slotNumber slot {slotField = value}

readSlotVitality :: Game s -> Player -> SlotNumber -> ST s Vitality
readSlotVitality game player slotNumber = do
  slot <- readSlot game player slotNumber
  return (slotVitality slot)

writeSlotVitality :: Game s -> Player -> SlotNumber -> Vitality -> ST s ()
writeSlotVitality game player slotNumber vitality = do
  slot <- readSlot game player slotNumber
  writeSlot game player slotNumber slot {slotVitality = vitality}

leftApply :: Game s -> Player -> Card -> SlotNumber -> ST s ()
leftApply game player card slotNumber = do
  value <- readSlotField game player slotNumber
  value' <- fmap (fromMaybe (CardValue I)) (apply game (CardValue card) value)
  writeSlotField game player slotNumber value'

rightApply :: Game s -> Player -> SlotNumber -> Card -> ST s ()
rightApply game player slotNumber card = do
  value <- readSlotField game player slotNumber
  value' <- fmap (fromMaybe (CardValue I)) (apply game value (CardValue card))
  writeSlotField game player slotNumber value'

-- TODO: Finish this
apply :: Game s -> Value -> Value -> ST s (Maybe Value)
apply game function value = do
  return (Just value)

--------------------------------------------------------------------------------

play :: Program -> Player -> IO ()
play program player = do
  game <- stToIO newGame
  when (player == Them) (getTheirMove game)
  runCCT $ do
    i <- begin program
    loop game i i
  where
    loop game i0 i
      | finished i = loop game i0 i0
      | otherwise = do
          i' <- next i
          liftIO $ do
            putOurMove game (fromJust (current i))
            getTheirMove game
          loop game i0 i'
  
putOurMove :: Game RealWorld -> Move -> IO ()
putOurMove game move =
  case move of
    ApplyL card slotNumber -> do
      stToIO (leftApply game Us card slotNumber)
      putStrLn (show (fromEnum LeftApplication))
      putStrLn (show card)
      putStrLn (show slotNumber)
    ApplyR slotNumber card -> do
      stToIO (rightApply game Us slotNumber card)
      putStrLn (show (fromEnum RightApplication))
      putStrLn (show slotNumber)
      putStrLn (show card)

getTheirMove :: Game RealWorld -> IO ()
getTheirMove game = do
  move <- fmap (toEnum . read) getLine
  case move of
    LeftApplication -> do
      card <- fmap read getLine
      slotNumber <- fmap read getLine
      stToIO (leftApply game Them card slotNumber)
    RightApplication -> do
      slotNumber <- fmap read getLine
      card <- fmap read getLine
      stToIO (rightApply game Them slotNumber card)

--------------------------------------------------------------------------------
