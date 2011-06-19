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
ifAppsAreUnderLimit appCounter action = do
  apps <- readSTRef appCounter
  if apps < 1000
    then action
    else return Nothing

incrementApps :: AppCounter s -> ST s ()
incrementApps appCounter = modifySTRef appCounter (+ 1)

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

--------------------------------------------------------------------------------

data AppContext = AsUsual | AsZombie
  deriving Show

applyValue :: AppContext -> Game s -> Player -> Value -> Value -> ST s (Maybe Value)
applyValue appContext game player valueF value =
  withFunction valueF $ \function -> do
    appCounter <- newAppCounter
    applyFunction appCounter appContext game player function value

applyFunction :: AppCounter s -> AppContext -> Game s -> Player -> Function -> Value -> ST s (Maybe Value)
applyFunction appCounter appContext game player function value =
  ifAppsAreUnderLimit appCounter $ do
    incrementApps appCounter
    case function of
      IFunction -> return (Just value)
      SuccFunction ->
        withInt value $ \int ->
          return (Just (IntValue (min (int + 1) 65535)))
      DblFunction ->
        withInt value $ \int ->
          return (Just (IntValue (min (int * 2) 65535)))
      GetFunction ->
        withSlotNumber value $ \slotNumber -> do
          alive <- isSlotAlive game player slotNumber
          if alive
            then fmap Just (readSlotField game player slotNumber)
            else return Nothing
      PutFunction -> return (Just (FunctionValue IFunction))
      SFunction -> return (Just (FunctionValue (SFunction1 value)))
      SFunction1 valueF -> return (Just (FunctionValue (SFunction2 valueF value)))
      SFunction2 valueF valueG ->
        withFunction valueF $ \functionF -> do
          maybeValueH <- applyFunction appCounter appContext game player functionF value
          withValue maybeValueH $ \valueH ->
            withFunction valueG $ \functionG -> do
              maybeValueY <- applyFunction appCounter appContext game player functionG value
              withValue maybeValueY $ \valueY ->
                withFunction valueH $ \functionH ->
                  applyFunction appCounter appContext game player functionH valueY
      KFunction -> return (Just (FunctionValue (KFunction1 value)))
      KFunction1 valueX -> return (Just valueX)
      IncFunction ->
        withSlotNumber value $ \slotNumber -> do
          vitality <- readSlotVitality game player slotNumber
          case appContext of
            AsUsual ->
              when (vitality > 0 && vitality < 65535) $
                writeSlotVitality game player slotNumber (vitality + 1)
            AsZombie ->
              when (vitality > 0) $
                writeSlotVitality game player slotNumber (vitality - 1)
          return (Just (FunctionValue IFunction))
      DecFunction ->
        withSlotNumber value $ \slotNumber -> do
          vitality <- readSlotVitality game (otherPlayer player) (255 - slotNumber)
          case appContext of
            AsUsual ->
              when (vitality > 0) $
                writeSlotVitality game (otherPlayer player) (255 - slotNumber) (vitality - 1)
            AsZombie ->
              when (vitality > 0 && vitality < 65535) $
                writeSlotVitality game (otherPlayer player) (255 - slotNumber) (vitality + 1)
          return (Just (FunctionValue IFunction))
      AttackFunction -> return (Just (FunctionValue (AttackFunction1 value)))
      AttackFunction1 valueI -> return (Just (FunctionValue (AttackFunction2 valueI value)))
      AttackFunction2 valueI valueJ ->
        withSlotNumber valueI $ \slotNumberI ->
          withInt value $ \int -> do
            vitalityV <- readSlotVitality game player slotNumberI
            if vitalityV >= int
              then do
                writeSlotVitality game player slotNumberI (vitalityV - int)
                withSlotNumber valueJ $ \slotNumberJ -> do
                  vitalityW <- readSlotVitality game (otherPlayer player) (255 - slotNumberJ)
                  when (vitalityW > 0) $ do
                    let int' = (int * 9) `div` 10
                    case appContext of
                      AsUsual ->
                        writeSlotVitality game (otherPlayer player) (255 - slotNumberJ) (max (vitalityW - int') 0)
                      AsZombie ->
                        writeSlotVitality game (otherPlayer player) (255 - slotNumberJ) (min (vitalityW + int') 65535)
                  return (Just (FunctionValue IFunction))
              else return Nothing
      HelpFunction -> return (Just (FunctionValue (HelpFunction1 value)))
      HelpFunction1 valueI -> return (Just (FunctionValue (HelpFunction2 valueI value)))
      HelpFunction2 valueI valueJ ->
        withSlotNumber valueI $ \slotNumberI ->
          withInt value $ \int -> do
            vitalityV <- readSlotVitality game player slotNumberI
            if vitalityV >= int
              then do
                writeSlotVitality game player slotNumberI (vitalityV - int)
                withSlotNumber valueJ $ \slotNumberJ -> do
                  vitalityW <- readSlotVitality game player slotNumberJ
                  when (vitalityW > 0) $ do
                    let int' = (int * 11) `div` 10
                    case appContext of
                      AsUsual ->
                        writeSlotVitality game player slotNumberJ (min (vitalityW + int') 65535)
                      AsZombie ->
                        writeSlotVitality game player slotNumberJ (max (vitalityW - int') 0)
                  return (Just (FunctionValue IFunction))
              else return Nothing
      CopyFunction ->
        withSlotNumber value $ \slotNumber ->
          fmap Just (readSlotField game (otherPlayer player) slotNumber)
      ReviveFunction ->
        withSlotNumber value $ \slotNumber -> do
          vitality <- readSlotVitality game player slotNumber
          when (vitality <= 0) $
            writeSlotVitality game player slotNumber 1
          return (Just (FunctionValue IFunction))
      ZombieFunction -> return (Just (FunctionValue (ZombieFunction1 value)))
      ZombieFunction1 valueI ->
        withSlotNumber valueI $ \slotNumber -> do
          alive <- isSlotAlive game (otherPlayer player) (255 - slotNumber)
          if not alive
            then do
              writeSlotField game (otherPlayer player) (255 - slotNumber) value
              writeSlotVitality game (otherPlayer player) (255 - slotNumber) (-1)
              return (Just (FunctionValue IFunction))
            else return Nothing

applyZombies :: Game s -> Player -> ST s ()
applyZombies game player =
  forM_ [0 .. 255] $ \slotNumber -> do
    zombie <- isSlotZombie game player slotNumber
    when zombie $ do
      value <- readSlotField game player slotNumber
      _ <- applyValue AsZombie game player value (FunctionValue IFunction)
      writeSlotField game player slotNumber (FunctionValue IFunction)

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
