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
import EvalEnv(Env, buildEvalEnv, lookupEvalEnv)

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
      env <- buildEvalEnv declarations
      (main, e) <- lookupEvalEnv name env
      case main of 
        (VarPat _ _) ->  evalExp env e
        _ -> fail (name ++ " not found or not the right from")


evalExp :: Monad m => [Env] -> Exp -> m Value
evalExp env (LitExp l _) = return $ LitVal l
evalExp env e = fail "not supported"

