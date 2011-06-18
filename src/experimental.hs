module Main where

import Control.Monad.ST (ST, stToIO)
import Data.Array.ST (STArray, newArray, readArray, writeArray)
import Data.Maybe (fromMaybe)
import GHC.Prim (RealWorld)
import System.Environment (getArgs)
import System.IO (BufferMode (..), hSetBuffering, stdin, stdout)

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

apply :: Game s -> Value -> Value -> ST s (Maybe Value)
apply game function value = do
  return (Just value)

--------------------------------------------------------------------------------

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  game <- stToIO newGame
  player <- fmap (toEnum . read . head) getArgs
  case player of
    Us -> putOurMove game
    Them -> getTheirMove game

putOurMove :: Game RealWorld -> IO ()
putOurMove game = do
  putStrLn "1"
  putStrLn "I"
  putStrLn "0"
  getTheirMove game

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
  putOurMove game

--------------------------------------------------------------------------------
