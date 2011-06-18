module Main where

import System.Environment (getArgs)
import System.IO (BufferMode (..), hSetBuffering, stdin, stdout)

import IgnorantCommon
import Common


mainProgram :: Program
mainProgram = Move (ApplyL I 0)

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  player <- fmap (toEnum . read . head) getArgs
  play mainProgram player
