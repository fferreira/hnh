module Compiler
{- TODO Debug only    
    (

    )
-}
    where

import Lexer
import Parser
import Text.PrettyPrint.Leijen
import Syntax

import qualified ParserMonad as P
import qualified TransformMonad as T

import ExprTransformer

import Data.List(intersperse)

programTransform :: Program -> (T.TransformResult Program, [Doc])
programTransform p = 
    let (res, docs)  = T.runTransform (correctPrecedence p 
                                       >>= toPrefix
                                       >>= return)
    in
      (res, (pretty p):docs) -- adding the original to the list

rawParse :: String -> P.ParseResult Program
rawParse = P.runParser parser


fileToProgram :: String -> Bool -> IO Doc
fileToProgram file showSteps = do contents <- readFile file 
                                  parsed <- return $ rawParse contents
                                  (programRes, docs) <- return $ runTransformations parsed
                                  program <- return $ checkTransformation programRes 
                                  doc <- return $ if showSteps then
                                                      printSteps docs
                                                  else
                                                      pretty program
                                  return doc

printSteps :: [Doc] -> Doc
printSteps docs = vsep $ intersperse (line <> pretty ">>> transforms to:" <> line) docs

runTransformations :: P.ParseResult Program -> (T.TransformResult Program, [Doc])
runTransformations (P.Ok _ p) = programTransform p
runTransformations (P.Failed p s) = error $ show (pretty p <+> pretty s)

checkTransformation :: T.TransformResult Program -> Program
checkTransformation (T.Ok program) = program
checkTransformation (T.Failed err) = error err

--- simpler compiler

compile program = case P.runParser parser program of
                    (P.Ok _ r) -> let (tran, hist) = programTransform r in
                                case tran of
                                  (T.Ok t) ->  pretty t
                                  (T.Failed s) -> pretty "Error" <> colon <+> pretty s
                    (P.Failed p s) -> pretty "Error:" <+> pretty s <+> pretty (show p)


