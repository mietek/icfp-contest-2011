module AwareCommon where

import Control.Monad (forM_, when)
import Control.Monad.CC (runCCT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.ST (ST, stToIO)
import Data.Array.ST (STArray, newArray, readArray, writeArray)
import Data.Maybe (fromJust, fromMaybe)
import Data.STRef (STRef, newSTRef, readSTRef, modifySTRef)
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

--------------------------------------------------------------------------------

isSlotAlive :: Game s -> Player -> SlotNumber -> ST s Bool
isSlotAlive game player slotNumber = do
  vitality <- readSlotVitality game player slotNumber
  return (isVitalityAlive vitality)

isSlotZombie :: Game s -> Player -> SlotNumber -> ST s Bool
isSlotZombie game player slotNumber = do
  vitality <- readSlotVitality game player slotNumber
  return (isVitalityZombie vitality)

--------------------------------------------------------------------------------

type AppCounter s = STRef s Int

newAppCounter :: ST s (AppCounter s)
newAppCounter = newSTRef 0

ifAppsAreUnderLimit :: AppCounter s -> ST s (Maybe Value) -> ST s (Maybe Value)
ifAppsAreUnderLimit counter action = do
  apps <- readSTRef counter
  if apps < 1000
    then action
    else return Nothing

incrementApps :: AppCounter s -> ST s ()
incrementApps counter = modifySTRef counter (+ 1)

--------------------------------------------------------------------------------

leftApply :: Game s -> Player -> Card -> SlotNumber -> ST s ()
leftApply game player card slotNumber = do
  value <- readSlotField game player slotNumber
  value' <- fmap (fromMaybe (FunctionValue IFunction)) (applyValue AsUsual game player (cardToValue card) value)
  writeSlotField game player slotNumber value'

rightApply :: Game s -> Player -> SlotNumber -> Card -> ST s ()
rightApply game player slotNumber card = do
  value <- readSlotField game player slotNumber
  value' <- fmap (fromMaybe (FunctionValue IFunction)) (applyValue AsUsual game player value (cardToValue card))
  writeSlotField game player slotNumber value'

applyZombies :: Game s -> Player -> ST s ()
applyZombies game player =
  forM_ [0 .. 255] $ \slotNumber -> do
    zombie <- isSlotZombie game player slotNumber
    when zombie $ do
      value <- readSlotField game player slotNumber
      _ <- applyValue AsZombie game player value (FunctionValue IFunction)
      writeSlotField game player slotNumber (FunctionValue IFunction)

--------------------------------------------------------------------------------

data AppContext = AsUsual | AsZombie
  deriving Show

applyValue :: AppContext -> Game s -> Player -> Value -> Value -> ST s (Maybe Value)
applyValue context game player valueF value =
  withFunction valueF $ \function -> do
    counter <- newAppCounter
    applyFunction counter context game player function value

applyFunction :: AppCounter s -> AppContext -> Game s -> Player -> Function -> Value -> ST s (Maybe Value)
applyFunction counter context game player function value =
  ifAppsAreUnderLimit counter $ do
    incrementApps counter
    case function of
      IFunction -> applyI value
      SuccFunction -> applySucc value
      DblFunction -> applyDbl value
      GetFunction -> applyGet game player value
      PutFunction -> applyPut value
      SFunction -> applyS value
      SFunction1 valueF -> applyS1 valueF value
      SFunction2 valueF valueG -> applyS2 counter context game player valueF valueG value
      KFunction -> applyK value
      KFunction1 valueX -> applyK1 valueX value
      IncFunction -> applyInc context game player value
      DecFunction -> applyDec context game player value
      AttackFunction -> applyAttack value
      AttackFunction1 valueI -> applyAttack1 valueI value
      AttackFunction2 valueI valueJ -> applyAttack2 context game player valueI valueJ value
      HelpFunction -> applyHelp value
      HelpFunction1 valueI -> applyHelp1 valueI value
      HelpFunction2 valueI valueJ -> applyHelp2 context game player valueI valueJ value
      CopyFunction -> applyCopy game player value
      ReviveFunction -> applyRevive game player value
      ZombieFunction -> applyZombie value
      ZombieFunction1 valueI -> applyZombie1 game player valueI value

--------------------------------------------------------------------------------

applyI :: Value -> ST s (Maybe Value)
applyI valueX = return (Just valueX)

applySucc :: Value -> ST s (Maybe Value)
applySucc valueN =
  withInt valueN $ \intN ->
    return (Just (IntValue (min (intN + 1) 65535)))

applyDbl :: Value -> ST s (Maybe Value)
applyDbl valueN =
  withInt valueN $ \intN ->
    return (Just (IntValue (min (intN * 2) 65535)))

applyGet :: Game s -> Player -> Value -> ST s (Maybe Value)
applyGet game player valueI =
  withSlotNumber valueI $ \slotNumberI -> do
    aliveI <- isSlotAlive game player slotNumberI
    if aliveI
      then fmap Just (readSlotField game player slotNumberI)
      else return Nothing

applyPut :: Value -> ST s (Maybe Value)
applyPut _ = return (Just (FunctionValue IFunction))

applyS :: Value -> ST s (Maybe Value)
applyS valueF = return (Just (FunctionValue (SFunction1 valueF)))

applyS1 :: Value -> Value -> ST s (Maybe Value)
applyS1 valueF valueG = return (Just (FunctionValue (SFunction2 valueF valueG)))

applyS2 :: AppCounter s -> AppContext -> Game s -> Player -> Value -> Value -> Value -> ST s (Maybe Value)
applyS2 counter context game player valueF valueG valueX =
  withFunction valueF $ \functionF -> do
    maybeValueH <- applyFunction counter context game player functionF valueX
    withValue maybeValueH $ \valueH ->
      withFunction valueG $ \functionG -> do
        maybeValueY <- applyFunction counter context game player functionG valueX
        withValue maybeValueY $ \valueY ->
          withFunction valueH $ \functionH ->
            applyFunction counter context game player functionH valueY

applyK :: Value -> ST s (Maybe Value)
applyK valueX = return (Just (FunctionValue (KFunction1 valueX)))

applyK1 :: Value -> Value -> ST s (Maybe Value)
applyK1 valueX _ = return (Just valueX)

applyInc :: AppContext -> Game s -> Player -> Value -> ST s (Maybe Value)
applyInc context game player valueI =
  withSlotNumber valueI $ \slotNumberI -> do
    vitalityV <- readSlotVitality game player slotNumberI
    case context of
      AsUsual ->
        when (vitalityV > 0 && vitalityV < 65535) $
          writeSlotVitality game player slotNumberI (vitalityV + 1)
      AsZombie ->
        when (vitalityV > 0) $
          writeSlotVitality game player slotNumberI (vitalityV - 1)
    return (Just (FunctionValue IFunction))

applyDec :: AppContext -> Game s -> Player -> Value -> ST s (Maybe Value)
applyDec context game player valueI =
  withSlotNumber valueI $ \slotNumberI -> do
    let oppositePlayer = swapPlayer player
        oppositeSlotNumberI = swapSlotNumber slotNumberI
    vitalityV <- readSlotVitality game oppositePlayer oppositeSlotNumberI
    when (vitalityV > 0) $
      case context of
        AsUsual ->
          writeSlotVitality game oppositePlayer oppositeSlotNumberI (vitalityV - 1)
        AsZombie ->
          when (vitalityV < 65535) $
            writeSlotVitality game oppositePlayer oppositeSlotNumberI (vitalityV + 1)
    return (Just (FunctionValue IFunction))

applyAttack :: Value -> ST s (Maybe Value)
applyAttack valueI = return (Just (FunctionValue (AttackFunction1 valueI)))

applyAttack1 :: Value -> Value -> ST s (Maybe Value)
applyAttack1 valueI valueJ = return (Just (FunctionValue (AttackFunction2 valueI valueJ)))

applyAttack2 :: AppContext -> Game s -> Player -> Value -> Value -> Value -> ST s (Maybe Value)
applyAttack2 context game player valueI valueJ valueN =
  withSlotNumber valueI $ \slotNumberI ->
    withInt valueN $ \intN -> do
      vitalityV <- readSlotVitality game player slotNumberI
      if vitalityV >= intN
        then do
          writeSlotVitality game player slotNumberI (vitalityV - intN)
          withSlotNumber valueJ $ \slotNumberJ -> do
            let oppositePlayer = swapPlayer player
                oppositeSlotNumberJ = swapSlotNumber slotNumberJ
                intN' = (intN * 9) `div` 10
            vitalityW <- readSlotVitality game oppositePlayer oppositeSlotNumberJ
            when (vitalityW > 0) $
              case context of
                AsUsual ->
                  writeSlotVitality game oppositePlayer oppositeSlotNumberJ (max (vitalityW - intN') 0)
                AsZombie ->
                  writeSlotVitality game oppositePlayer oppositeSlotNumberJ (min (vitalityW + intN') 65535)
            return (Just (FunctionValue IFunction))
        else return Nothing

applyHelp :: Value -> ST s (Maybe Value)
applyHelp valueI = return (Just (FunctionValue (HelpFunction1 valueI)))

applyHelp1 :: Value -> Value -> ST s (Maybe Value)
applyHelp1 valueI valueJ = return (Just (FunctionValue (HelpFunction2 valueI valueJ)))

applyHelp2 :: AppContext -> Game s -> Player -> Value -> Value -> Value -> ST s (Maybe Value)
applyHelp2 context game player valueI valueJ valueN =  
  withSlotNumber valueI $ \slotNumberI ->
    withInt valueN $ \intN -> do
      vitalityV <- readSlotVitality game player slotNumberI
      if vitalityV >= intN
        then do
          writeSlotVitality game player slotNumberI (vitalityV - intN)
          withSlotNumber valueJ $ \slotNumberJ -> do            
            let intN' = (intN * 11) `div` 10
            vitalityW <- readSlotVitality game player slotNumberJ
            when (vitalityW > 0) $ do
              case context of
                AsUsual ->
                  writeSlotVitality game player slotNumberJ (min (vitalityW + intN') 65535)
                AsZombie ->
                  writeSlotVitality game player slotNumberJ (max (vitalityW - intN') 0)
            return (Just (FunctionValue IFunction))
        else return Nothing

applyCopy :: Game s -> Player -> Value -> ST s (Maybe Value)
applyCopy game player valueI =
  withSlotNumber valueI $ \slotNumberI -> do
    let oppositePlayer = swapPlayer player
    fmap Just (readSlotField game oppositePlayer slotNumberI)

applyRevive :: Game s -> Player -> Value -> ST s (Maybe Value)
applyRevive game player valueI =
  withSlotNumber valueI $ \slotNumberI -> do
    alive <- isSlotAlive game player slotNumberI
    when (not alive) $
      writeSlotVitality game player slotNumberI 1
    return (Just (FunctionValue IFunction))

applyZombie :: Value -> ST s (Maybe Value)
applyZombie valueI = return (Just (FunctionValue (ZombieFunction1 valueI)))

applyZombie1 :: Game s -> Player -> Value -> Value -> ST s (Maybe Value)
applyZombie1 game player valueI valueX =
  withSlotNumber valueI $ \slotNumberI -> do
    let oppositePlayer = swapPlayer player
        oppositeSlotNumberI = swapSlotNumber slotNumberI
    alive <- isSlotAlive game oppositePlayer oppositeSlotNumberI
    if not alive
      then do
        writeSlotField game oppositePlayer oppositeSlotNumberI valueX
        writeSlotVitality game oppositePlayer oppositeSlotNumberI (-1)
        return (Just (FunctionValue IFunction))
      else return Nothing

--------------------------------------------------------------------------------

withInt :: (Monad m) => Value -> (SlotNumber -> m (Maybe Value)) -> m (Maybe Value)
withInt (IntValue int) action = action int
withInt _ _ = return Nothing

withSlotNumber :: (Monad m) => Value -> (SlotNumber -> m (Maybe Value)) -> m (Maybe Value)
withSlotNumber value action =
  withInt value $ \int ->
    if isValidSlotNumber int
      then action int
      else return Nothing

withFunction :: (Monad m) => Value -> (Function -> m (Maybe Value)) -> m (Maybe Value)
withFunction (FunctionValue function) action = action function
withFunction _ _ = return Nothing

withValue :: (Monad m) => Maybe Value -> (Value -> m (Maybe Value)) -> m (Maybe Value)
withValue (Just value) action = action value
withValue _ _ = return Nothing

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
putOurMove game move = do
  stToIO (applyZombies game Us)
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
  stToIO (applyZombies game Them)
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
