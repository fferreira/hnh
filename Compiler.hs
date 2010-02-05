module Compiler
    (
     compile,
     main
    )
    where

import Lexer
import Layout
import Parser

import SamplePrograms -- DEBUG only


compile input = (parser . layout . lexer) input

compileDeclaration = map compile sampleDeclarations

main = do
  interact (show . compile)