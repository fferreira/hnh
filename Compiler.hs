module Compiler
{- TODO Debug only    
    (
     compile,
     main
    )
-}
    where

import Lexer
import Layout
import Parser
import ParserMonad

import SamplePrograms -- DEBUG only

compile program = runParser parser program


compileDeclaration = map compile sampleDeclarations

fileCompiler = do contents <- readFile "program.hasnt"
                  return $ compile contents
main = do
  interact (show . compile)


