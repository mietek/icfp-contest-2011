module Main where

import Control.Monad.ST (ST, stToIO)
import Data.Ix (Ix)
import Data.Array.ST (STArray, newArray, readArray, writeArray)
import Data.Maybe (fromMaybe)
import GHC.Prim (RealWorld)
import System.Environment (getArgs)
import System.IO (BufferMode (..), hSetBuffering, stdin, stdout)


--------------------------------------------------------------------------------

data Player = Us | Them
  deriving (Enum, Eq, Ix, Ord, Show)

swapPlayer :: Player -> Player
swapPlayer Us = Them
swapPlayer Them = Us

--------------------------------------------------------------------------------

data Move = LeftApplication | RightApplication
  deriving (Eq, Ix, Ord, Show)

instance Enum Move where
  toEnum 1 = LeftApplication
  toEnum 2 = RightApplication
  toEnum _ = error "Move.toEnum: failure"
  fromEnum LeftApplication = 1
  fromEnum RightApplication = 2

--------------------------------------------------------------------------------

type Game s = STArray s (Player, SlotNumber) Slot

newGame :: ST s (Game s)
newGame = newArray ((Us, 0), (Them, 255)) newSlot

--------------------------------------------------------------------------------

data Slot = Slot {
  slotField :: Value,
  slotVitality :: Vitality
} deriving Show

newSlot :: Slot
newSlot = Slot {slotField = CardValue I, slotVitality = 10000}

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

type Vitality = Int

isValidVitality :: Vitality -> Bool
isValidVitality n
  | n >= -1 && n <= 65535 = True
  | otherwise = False

--------------------------------------------------------------------------------

data Value = IntValue Int | CardValue Card
  deriving Show

isValidValue :: Int -> Bool
isValidValue n
  | n >= 0 && n <= 65535 = True
  | otherwise = False

--------------------------------------------------------------------------------

type SlotNumber = Int

isValidSlotNumber :: SlotNumber -> Bool
isValidSlotNumber n
  | n >= 0 && n <= 255 = True
  | otherwise = False

isSlotAlive :: Slot -> Bool
isSlotAlive s = slotVitality s > 0

--------------------------------------------------------------------------------

data Card = I | Zero | Succ | Dbl | Get | Put | S | K | Inc | Dec | Attack | Help | Copy | Revive | Zombie

instance Show Card where
  show card = 
    case card of
      I -> "I"
      Zero -> "zero"
      Succ -> "succ"
      Dbl -> "dbl"
      Get -> "get"
      Put -> "put"
      S -> "S"
      K -> "K"
      Inc -> "inc"
      Dec -> "dec"
      Attack -> "attack"
      Help -> "help"
      Copy -> "copy"
      Revive -> "revive"
      Zombie -> "zombie"

instance Read Card where
  readsPrec _ string = [(card, rest)]
    where
      (lexeme, rest) = head (lex string)
      card = case lexeme of
        "I" -> I
        "zero" -> Zero
        "succ" -> Succ
        "dbl" -> Dbl
        "get" -> Get
        "put" -> Put
        "S" -> S
        "K" -> K
        "inc" -> Inc
        "dec" -> Dec
        "attack" -> Attack
        "help" -> Help
        "copy" -> Copy
        "revive" -> Revive
        "zombie" -> Zombie
        _ -> error "readCard: no parse"

--------------------------------------------------------------------------------

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  game <- stToIO newGame
  player <- fmap (toEnum . read . head) getArgs
  if player == Us
    then putOurMove game
    else getTheirMove game

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
