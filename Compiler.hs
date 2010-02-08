module Compiler
{- DEBUG Only
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

compile input = runParser (parser ((layout . lexer) input)) ""

compileDeclaration = map compile sampleDeclarations

main = do
  interact (show . compile)

