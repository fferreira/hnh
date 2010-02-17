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

import ExprTransformer

import SamplePrograms -- DEBUG only


rawparse program = runParser parser program

compile program = case runParser parser program of
                    (Ok _ r) -> pretty r
                    (Failed p s) -> pretty "Error:" <//> pretty s <//> pretty (show p)


compileDeclaration = vsep $  map compile sampleDeclarations

fileCompiler = do contents <- readFile "program.hnh"
                  return $ compile contents

fileToProgram = do contents <- readFile "program.hnh"
                   return $ case runParser parser contents of
                              (Ok _ p) -> buildPrecedenceTable p
                              othewise -> undefined

