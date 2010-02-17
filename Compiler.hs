module Compiler
{- TODO Debug only    
    (
     compile,
     main
    )
-}
    where

import Lexer
import Parser
import ParserMonad
import Text.PrettyPrint.Leijen

import SamplePrograms -- DEBUG only

compile program = case runParser parser program of
                    (Ok _ r) -> pretty r
                    (Failed p s) -> pretty "Error:" <//> pretty s <//> pretty (show p)


compileDeclaration = map compile sampleDeclarations

fileCompiler = do contents <- readFile "program.hasnt"
                  return $ compile contents

