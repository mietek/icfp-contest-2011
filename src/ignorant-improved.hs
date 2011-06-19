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

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  player <- fmap (toEnum . read . head) getArgs
  play mainProgram player

--------------------------------------------------------------------------------
