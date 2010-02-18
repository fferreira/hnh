module Main where

import Compiler

main = do
  p <- fileCompiler
  print p