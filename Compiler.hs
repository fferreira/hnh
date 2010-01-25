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


compile input = (parse . layout . lexer) input



main = do
  interact (show . compile)