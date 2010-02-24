module Eval where --TODO add exported methods

import Parser
import Text.PrettyPrint.Leijen
import Syntax

import ExprTransformer(correctPrecedence, toPrefix)
import TreeSimplify(funToLambda)

import qualified ParserMonad as P
import qualified TransformMonad as T
import ErrorMonad

import Value
import EvalEnv(Env)

import Data.List(intersperse)


evaluationTransform :: Program -> (T.TransformResult Program, [Doc])
evaluationTransform p = 
    let (res, docs)  = T.runTransform (correctPrecedence p 
                                       >>= toPrefix
                                       >>= funToLambda
                                       >>= return)
    in
      (res, (pretty p):docs) -- adding the original to the list

rawParse :: String -> P.ParseResult Program
rawParse = P.runParser parser

runTransformations :: P.ParseResult Program -> (T.TransformResult Program, [Doc])
runTransformations (P.Ok _ p) = evaluationTransform p
runTransformations (P.Failed p s) = error $ show (pretty p <+> pretty s)

checkTransformation :: T.TransformResult Program -> Program
checkTransformation (T.Ok program) = program
checkTransformation (T.Failed err) = error err

loadAndEval :: String -> Name -> Bool -> IO Doc
loadAndEval file main showSteps = do contents <- readFile file 
                                     parsed <- return $ rawParse contents
                                     (programRes, docs) <- return $ runTransformations parsed
                                     program <- return $ checkTransformation programRes 
                                     doc <- return $ if showSteps then
                                                         printSteps docs
                                                     else
                                                         pretty $ eval program main
                                     return doc

printSteps :: [Doc] -> Doc
printSteps docs = 
    let
        result = vsep $ intersperse (line <> pretty ">>> transforms to:" <> line) docs
        epigraph = pretty "Number of phases:" <> pretty ((length docs) - 1)
    in
      result <> line <> epigraph

--------------------------------------------------------------------------------------



-- name should be a single var pattern i.e. main = 1 + 1
eval :: Program -> Name -> ErrorM Value
eval (Program declarations) name =
    do
      mainDecl <- findDeclarations name declarations
      main <- return (head $ mainDecl)
      case main of 
        (PatBindDcl (VarPat _ _) rhs) ->  evalRhs rhs
        _ -> fail (name ++ " not found or not the right from")

findDeclarations :: Monad m => Name -> [Declaration] -> m [Declaration]
findDeclarations name declarations = fail "not implemented 1" 

evalRhs :: Monad m => Rhs -> m Value
evalRhs rhs = fail "not implemented"


buildEvalEnv :: Monad m => [Declaration] -> m [Env]
buildEvalEnv decls = mapM declToValue decls
    where
      declToValue (PatBindDcl pat rhs) = undefined