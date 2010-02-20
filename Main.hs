module Main where

import Compiler
import System.Environment(getArgs)

main = do
  args <- getArgs
  p <- fileToProgram (args!!0) (if ((length args) >= 2) then True else False)
  print p