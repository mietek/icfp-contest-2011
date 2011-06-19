module Common where

import Control.Monad.CC (MonadDelimitedCont, reset, shift)
import Data.Ix (Ix)
import Data.List (foldl')


--------------------------------------------------------------------------------

data Player = Us | Them
  deriving (Enum, Eq, Ix, Ord, Show)

--------------------------------------------------------------------------------

data MoveType = LeftApplication | RightApplication
  deriving (Eq, Ix, Ord, Show)

instance Enum MoveType where
  toEnum 1 = LeftApplication
  toEnum 2 = RightApplication
  toEnum _ = error "Move.toEnum: failure"
  fromEnum LeftApplication = 1
  fromEnum RightApplication = 2

--------------------------------------------------------------------------------

data Move = ApplyL Card SlotNumber | ApplyR SlotNumber Card
  deriving Show

--------------------------------------------------------------------------------

data Slot = Slot {
  slotField :: Value,
  slotVitality :: Vitality
} deriving Show

newSlot :: Slot
newSlot = Slot {slotField = FunctionValue IFunction, slotVitality = 10000}

--------------------------------------------------------------------------------

data Function =
    IFunction
  | SuccFunction
  | DblFunction
  | GetFunction
  | PutFunction
  | SFunction | SFunction1 Value | SFunction2 Value Value
  | KFunction | KFunction1 Value
  | IncFunction
  | DecFunction
  | AttackFunction | AttackFunction1 Value | AttackFunction2 Value Value
  | HelpFunction | HelpFunction1 Value | HelpFunction2 Value Value
  | CopyFunction
  | ReviveFunction
  | ZombieFunction | ZombieFunction1 Value
  deriving Show

data Value = IntValue Int | FunctionValue Function
  deriving Show

cardToValue :: Card -> Value
cardToValue card =
  case card of
    I -> FunctionValue IFunction
    Zero -> IntValue 0
    Succ -> FunctionValue SuccFunction
    Dbl -> FunctionValue DblFunction
    Get -> FunctionValue GetFunction
    Put -> FunctionValue PutFunction
    S -> FunctionValue SFunction
    K -> FunctionValue KFunction
    Inc -> FunctionValue IncFunction
    Dec -> FunctionValue DecFunction
    Attack -> FunctionValue AttackFunction
    Help -> FunctionValue HelpFunction
    Copy -> FunctionValue CopyFunction
    Revive -> FunctionValue ReviveFunction
    Zombie -> FunctionValue ZombieFunction

isValidIntValue :: Int -> Bool
isValidIntValue n
  | n >= 0 && n <= 65535 = True
  | otherwise = False

--------------------------------------------------------------------------------

type Vitality = Int

isValidVitality :: Vitality -> Bool
isValidVitality n
  | n >= -1 && n <= 65535 = True
  | otherwise = False

isVitalityAlive :: Vitality -> Bool
isVitalityAlive n
  | n > 0 = True
  | otherwise = False

--------------------------------------------------------------------------------

type SlotNumber = Int

isValidSlotNumber :: SlotNumber -> Bool
isValidSlotNumber n
  | n >= 0 && n <= 255 = True
  | otherwise = False

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
