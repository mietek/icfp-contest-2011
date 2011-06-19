module Main where

import System.Environment (getArgs)
import System.IO (BufferMode (..), hSetBuffering, stdin, stdout)

import IgnorantCommon
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

mainProgram :: Program
mainProgram = Move (ApplyL I 0)

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

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  player <- fmap (toEnum . read . head) getArgs
  play mainProgram player

--------------------------------------------------------------------------------
