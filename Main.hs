module Main where

import Eval
import System.Environment(getArgs)

main = do
  args <- getArgs
  p <- loadAndEval (args!!0) (args!!1) (if ((length args) >= 3) then True else False)
  print p