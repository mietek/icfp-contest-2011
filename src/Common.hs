module Common where

import Control.Monad.CC (MonadDelimitedCont, reset, shift)
import Data.Ix (Ix)
import Data.List (foldl')


--------------------------------------------------------------------------------

data Player = Us | Them
  deriving (Enum, Eq, Ix, Ord, Show)

otherPlayer :: Player -> Player
otherPlayer Us = Them
otherPlayer Them = Us

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

isVitalityZombie :: Vitality -> Bool
isVitalityZombie n
  | n == -1 = True
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

numberToCards :: Int -> [Card]
numberToCards number =
  let (number', remainder) = number `divMod` 2 in
    if number' > 0
      then if remainder == 1
        then Succ : Dbl : numberToCards number'
        else Dbl : numberToCards number'
      else if remainder == 1
        then [Succ]
        else []

numberToMoves :: Int -> SlotNumber -> [Move]
numberToMoves number slotNumber =
  concatMap (\card -> [
    ApplyL K slotNumber,
    ApplyL S slotNumber,
    ApplyR slotNumber card
  ]) cards ++ [
    ApplyR slotNumber Zero
  ]
  where
    cards = numberToCards number

movesToProgram :: [Move] -> Program
movesToProgram = Concat . map Move

putProgram :: Program -> IO ()
putProgram program =
  forProgram program (\move -> putStrLn (show move))

--------------------------------------------------------------------------------

-- assuming that given slot is "empty" eg. contains only Id
valueToProgram :: Value -> SlotNumber -> Program
valueToProgram (FunctionValue func) n = case func of
	IFunction -> Move (ApplyL I n)
	SuccFunction -> Move (ApplyL Succ n)
	DblFunction -> Move (ApplyL Dbl n)
	GetFunction -> Move (ApplyL Get n)
	PutFunction -> Move (ApplyL Put n)
	IncFunction -> Move (ApplyL Inc n)
	DecFunction -> Move (ApplyL Dec n)
	CopyFunction -> Move (ApplyL Copy n)
	ReviveFunction -> Move (ApplyL Revive n)
	SFunction -> Move (ApplyL S n)
	SFunction1 x -> Concat [valueToProgram x n, Move (ApplyL S n)]
	SFunction2 x y -> Concat [valueToProgram x n, Move (ApplyL S n), appendValueToProgram y n]
	KFunction -> Move (ApplyL K n)
	KFunction1 x -> Concat [valueToProgram x n, Move (ApplyL K n)]
	AttackFunction -> Move (ApplyL Attack n)
	AttackFunction1 x -> Concat [valueToProgram x n, Move (ApplyL Attack n)]
	AttackFunction2 x y -> Concat [valueToProgram x n, Move (ApplyL Attack n), appendValueToProgram y n]
	HelpFunction -> Move (ApplyL Help n)
	HelpFunction1 x -> Concat [valueToProgram x n, Move (ApplyL Help n)]
	HelpFunction2 x y -> Concat [valueToProgram x n, Move (ApplyL Help n), appendValueToProgram y n]
	ZombieFunction -> Move (ApplyL Zombie n)
	ZombieFunction1 x -> Concat [valueToProgram x n, Move (ApplyL Zombie n)]
	
valueToProgram (IntValue x) n = movesToProgram(numberToMoves(x)(n))
	
appendValueToProgram :: Value -> SlotNumber -> Program
-- to be replaced with REAL behaviour
appendValueToProgram f n = Move (ApplyL I n)
