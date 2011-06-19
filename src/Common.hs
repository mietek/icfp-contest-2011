module Common where

import Control.Monad.CC (MonadDelimitedCont, reset, shift)
import Data.Ix (Ix)
import Data.List (foldl')


--------------------------------------------------------------------------------

data Player = Us | Them
  deriving (Enum, Eq, Ix, Ord, Show)

swapPlayer :: Player -> Player
swapPlayer Us = Them
swapPlayer Them = Us

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

swapSlotNumber :: SlotNumber -> SlotNumber
swapSlotNumber n = 255 - n

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

newIterator :: MonadDelimitedCont p s m => Program -> m (Iterator m)
newIterator program =
  reset $ \delimiter -> do
    forProgram program $ \move ->
      shift delimiter $ \continuation ->
        return (Current move (continuation (return ())))
    return Done

iteratorCurrent :: Iterator m -> Maybe Move
iteratorCurrent Done = Nothing
iteratorCurrent (Current move _) = Just move

nextIterator :: Monad m => Iterator m -> m (Iterator m)
nextIterator Done = return Done
nextIterator (Current _ iterator) = iterator

isIteratorDone :: Iterator m -> Bool
isIteratorDone Done = True
isIteratorDone _ = False

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
  IFunction -> Move (ApplyR n I)
  SuccFunction -> Move (ApplyR n Succ)
  DblFunction -> Move (ApplyR n Dbl)
  GetFunction -> Move (ApplyR n Get)
  PutFunction -> Move (ApplyR n Put)
  IncFunction -> Move (ApplyR n Inc)
  DecFunction -> Move (ApplyR n Dec)
  CopyFunction -> Move (ApplyR n Copy)
  ReviveFunction -> Move (ApplyR n Revive)
  SFunction -> Move (ApplyR n S)
  SFunction1 x -> Concat [valueToProgram x n, Move (ApplyL S n)]
  SFunction2 x y -> Concat [valueToProgram x n, Move (ApplyL S n), valueToArgumentProgram y n]
  KFunction -> Move (ApplyR n K)
  KFunction1 x -> Concat [valueToProgram x n, Move (ApplyL K n)]
  AttackFunction -> Move (ApplyR n Attack)
  AttackFunction1 x -> Concat [valueToProgram x n, Move (ApplyL Attack n)]
  AttackFunction2 x y -> Concat [valueToProgram x n, Move (ApplyL Attack n), valueToArgumentProgram y n]
  HelpFunction -> Move (ApplyR n Help)
  HelpFunction1 x -> Concat [valueToProgram x n, Move (ApplyL Help n)]
  HelpFunction2 x y -> Concat [valueToProgram x n, Move (ApplyL Help n), valueToArgumentProgram y n]
  ZombieFunction -> Move (ApplyR n Zombie)
  ZombieFunction1 x -> Concat [valueToProgram x n, Move (ApplyL Zombie n)]
  
valueToProgram (IntValue x) n = movesToProgram(numberToMoves(x)(n))
  
valueToArgumentProgram :: Value -> SlotNumber -> Program
valueToArgumentProgram (FunctionValue func) n = case func of
  IFunction -> Move (ApplyR n I)
  SuccFunction -> Move (ApplyR n Succ)
  DblFunction -> Move (ApplyR n Dbl)
  GetFunction -> Move (ApplyR n Get)
  PutFunction -> Move (ApplyR n Put)
  IncFunction -> Move (ApplyR n Inc)
  DecFunction -> Move (ApplyR n Dec)
  CopyFunction -> Move (ApplyR n Copy)
  ReviveFunction -> Move (ApplyR n Revive)
  SFunction -> Move (ApplyR n S)
  KFunction -> Move (ApplyR n K)
  AttackFunction -> Move (ApplyR n Attack)
  HelpFunction -> Move (ApplyR n Help)
  ZombieFunction -> Move (ApplyR n Zombie)
  SFunction1 x -> Concat [Move(ApplyL K n), Move(ApplyL S n), Move(ApplyR n S), valueToArgumentProgram x n]
  KFunction1 x -> Concat [Move(ApplyL K n), Move(ApplyL S n), Move(ApplyR n K), valueToArgumentProgram x n]
  AttackFunction1 x -> Concat [Move(ApplyL K n), Move(ApplyL S n), Move(ApplyR n Attack), valueToArgumentProgram x n]
  HelpFunction1 x -> Concat [Move(ApplyL K n), Move(ApplyL S n), Move(ApplyR n Help), valueToArgumentProgram x n]
  ZombieFunction1 x -> Concat [Move(ApplyL K n), Move(ApplyL S n), Move(ApplyR n Zombie), valueToArgumentProgram x n] 
  SFunction2 x y -> Concat [Move(ApplyL K n), Move(ApplyL S n), Move(ApplyL K n), Move(ApplyL S n), 
                            Move(ApplyR n S), valueToArgumentProgram x n, valueToArgumentProgram y n]
  AttackFunction2 x y -> Concat [Move(ApplyL K n), Move(ApplyL S n), Move(ApplyL K n), Move(ApplyL S n), 
                            Move(ApplyR n Attack), valueToArgumentProgram x n, valueToArgumentProgram y n]
  HelpFunction2 x y -> Concat [Move(ApplyL K n), Move(ApplyL S n), Move(ApplyL K n), Move(ApplyL S n), 
                            Move(ApplyR n Help), valueToArgumentProgram x n, valueToArgumentProgram y n]

valueToArgumentProgram (IntValue x) n = movesToProgram(numberToMoves(x)(n))

--------------------------------------------------------------------------------
compose :: Value -> Value -> Value
compose f g = FunctionValue (SFunction2 (FunctionValue (KFunction1 f)) g)

-- flattenProgram :: Program -> Program
-- flattenProgram (Move x) = Move x
-- flattenProgram (Concat [p]) = flattenProgram p
-- flattenProgram (Concat ((Concat ps):pss)) = flattenProgram (Concat (ps ++ pss))
-- flattenProgram (Concat (p:ps)) = Concat (p:ps') where Concat ps' = flattenProgram(Concat ps)
-- flattenProgram (Replicate n ps) = Replicate n ps' where Concat ps' = flattenProgram(Concat ps)

lazyGet :: SlotNumber -> Value
lazyGet n = FunctionValue (SFunction2 (FunctionValue IFunction) (FunctionValue (KFunction1 (IntValue n))))

-- adds another argument lazily loaded with get
-- should be used on functions with arity>1 when first argument is already set
addDelayedArgument :: Value -> SlotNumber -> Value
addDelayedArgument f n = FunctionValue (SFunction2 (f) (lazyGet n))

lazyHelp :: SlotNumber -> SlotNumber -> SlotNumber -> Value
lazyHelp a b c = addDelayedArgument help2 c where
	help2 = addDelayedArgument help1 b
	help1 = compose (FunctionValue HelpFunction) (lazyGet a)

lazyAttack :: SlotNumber -> SlotNumber -> SlotNumber -> Value
lazyAttack a b c = addDelayedArgument attack2 c where
	attack2 = addDelayedArgument attack1 b
	attack1 = compose (FunctionValue AttackFunction) (lazyGet a)
	
lazyZombie :: SlotNumber -> SlotNumber -> Value
lazyZombie a b = addDelayedArgument zombie' b where
	zombie' = compose (FunctionValue ZombieFunction) (lazyGet a)

--------------------------------------------------------------------------------
--in these functions last parameter always denotes slot number in which we build 

setNumber :: Int -> SlotNumber -> Program
setNumber number destination = valueToProgram (IntValue number) destination

setLazyHelper :: SlotNumber -> SlotNumber -> SlotNumber -> SlotNumber -> Program
setLazyHelper sacrificeSlotNum helpedSlotNum power destination = valueToProgram (lazyHelp sacrificeSlotNum helpedSlotNum power) destination

setLazyAttacker :: SlotNumber -> SlotNumber -> SlotNumber -> SlotNumber -> Program
setLazyAttacker sacrificeSlotNum opponentSlotNum power destination = valueToProgram (lazyAttack sacrificeSlotNum opponentSlotNum power) destination

setLazyZombie :: SlotNumber -> SlotNumber -> SlotNumber -> Program
setLazyZombie opponentSlotNum embeddedFunction destination = valueToProgram (lazyZombie opponentSlotNum embeddedFunction) destination

copyFrom :: SlotNumber -> SlotNumber -> Program
copyFrom source destination = Concat [(setNumber source destination), Move (ApplyL Get destination)]

erase :: SlotNumber -> Program
erase slotNum = Move (ApplyL Put slotNum)

increment :: SlotNumber -> Program
increment slotNum = Move (ApplyL Succ slotNum)

activate :: SlotNumber -> Program
activate slotNum = Move (ApplyR slotNum Get)
