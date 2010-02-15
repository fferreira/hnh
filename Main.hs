module Main where

import Compiler

main = do
  p <- fileCompiler
  putStrLn (show p)