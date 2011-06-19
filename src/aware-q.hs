module Main where

import System.Environment (getArgs)
import System.IO (BufferMode (..), hSetBuffering, stdin, stdout)

import AwareCommon
import Common


-- 1 zero
-- for [0 .. 255]
--     for [0 .. 9999]
--         0 zero
--         succ 0
--         get 0
--         dec 0
--     succ 1

mainProgram :: Program
mainProgram =
  Concat [
    Move (ApplyR 1 Zero),
    Replicate 256 [
      Replicate 10000 [
        Move (ApplyR 0 Zero),
        Move (ApplyL Succ 0),
        Move (ApplyL Get 0),
        Move (ApplyL Dec 0)
      ],
      Move (ApplyL Succ 1)
    ]
  ]

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  player <- fmap (toEnum . read . head) getArgs
  play mainProgram player
