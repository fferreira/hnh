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


main = do
  interact (show . compile)


