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
	 setNumber 9999 3,
	 setNumber (10000+9999*11 `div` 10-1) 5,
	 setNumber 0 1,
	 setNumber 1 2,
         valueToProgram (FunctionValue (SFunction2 (lazyHelp 1 2 3) (lazyHelp 2 1 5))) 4,
         valueToProgram (FunctionValue (SFunction2 (lazyAttack 1 1 7) (lazyHelp 1 2 7))) 6,
	 setNumber 11112 7,
	 Replicate 128
		 [ copyFrom 6 0,
		 activate 0,
		 Move (ApplyL Succ 1),
		 Move (ApplyL Succ 1),
		 Move (ApplyL Succ 2),
		 Move (ApplyL Succ 2)],
         Replicate 10000 [Move (ApplyL I 0)]
  ]

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  player <- fmap (toEnum . read . head) getArgs
  play mainProgram player
