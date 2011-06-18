module Common where

import Data.Ix (Ix)


--------------------------------------------------------------------------------

data Player = Us | Them
  deriving (Enum, Eq, Ix, Ord, Show)

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

data Slot = Slot {
  slotField :: Value,
  slotVitality :: Vitality
} deriving Show

newSlot :: Slot
newSlot = Slot {slotField = CardValue I, slotVitality = 10000}

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
        _ -> error "Card.readsPrec: failure"

--------------------------------------------------------------------------------
