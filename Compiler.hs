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
import Syntax

import qualified TransformMonad as TM

import ExprTransformer
import SamplePrograms -- DEBUG only

import System.IO.Unsafe(unsafePerformIO)  -- DEBUG!!!! -- TODO remove!


fakeTransform :: Program -> TM.TransformM Program
fakeTransform p = TM.transformOk p

programTransform :: Program -> (TM.TransformResult Program, [Doc])
programTransform p = 
    TM.runTransform (correctPrecedence p 
                     >>= fakeTransform 
                     >>= return)

rawparse = runParser parser

compile program = case runParser parser program of
                    (Ok _ r) -> let (tran, hist) = programTransform r in
                                case tran of
                                  (TM.Ok t) -> pretty (length hist)
                                  (TM.Failed s) -> pretty "Error" <> colon <+> pretty s
                    (Failed p s) -> pretty "Error:" <//> pretty s <//> pretty (show p)


compileDeclaration = vsep $  map compile sampleDeclarations

fileCompiler = do contents <- readFile "program.hnh"
                  return $ compile contents

fileToProgram :: String -> IO Doc
fileToProgram file = do contents <- readFile file -- "program.hnh"
                        return $ case runParser parser contents of
                                   (Ok _ p) -> 
                                       let 
                                           (tran, hist) = programTransform p
                                           p' = case tran of
                                                  (TM.Ok t) -> t
                                                  _ -> error "pum"
                                       in pretty "No correction"
                                                     <$> pretty p 
                                                     <$> pretty "-----------------" <$> pretty "Corrected"
                                                     <$> pretty p'
                                   (Failed p s) -> pretty "Error:" <//> pretty s <//> pretty (show p)
                              
