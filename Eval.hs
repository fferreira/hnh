module Eval where --TODO add exported methods

import Parser
import Text.PrettyPrint.Leijen
import Syntax

import ExprTransformer(correctPrecedence, toPrefix)
import TreeSimplify(funToLambda)

import qualified ParserMonad as P
import qualified TransformMonad as T
import ErrorMonad

import EvalEnv(Env, buildEvalEnv, lookupEvalEnv)

import Data.List(intersperse)
import Control.Monad.State

import Tools


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
                                                         pretty $ evalState (eval program main) []
                                     return doc

printSteps :: [Doc] -> Doc
printSteps docs = 
    let
        result = vsep $ intersperse (line <> pretty ">>> transforms to:" <> line) docs
        epigraph = pretty "Number of phases:" <> pretty ((length docs) - 1)
    in
      result <> line <> epigraph

--------------------------------------------------------------------------------------

type EvalState = [Env]


-- name should be a single var pattern i.e. main = 1 + 1
eval :: Program -> Name -> State EvalState Exp
eval (Program declarations) name =
    do
      env <- buildEvalEnv declarations
      put env
      env' <- mapM evalPat env
      put env'
      (main, e) <- lookupEvalEnv name env'
      case main of 
        (VarPat _ _) ->  evalExp e
        _ -> fail (name ++ " not found or not the right from")


evalExp :: Exp -> State EvalState Exp
evalExp e@(LitExp l _) = return e
evalExp e@(ConExp c t) = return e
evalExp (VarExp n _) = 
    do
      env <- get
      (p, e) <- lookupEvalEnv n env
      evalExp e

evalExp (IfExp e1 e2 e3 _) =
    do
      e1' <- evalExp e1
      res <- case e1' of
               (ConExp "True" _) -> evalExp e2
               (ConExp "False" _) -> evalExp e3
               _ -> fail "if condition not boolean"
      return res

evalExp (LetExp decls e _) =
    do
      env <- get
      env' <- buildEvalEnv decls -- get env'
      mapM evalPat env'

      e' <- evalExp e
      put env -- restores the original environment
      return e'


evalExp (ParensExp e _) = evalExp e

evalExp (InfixOpExp _ _) = fail "InfixOpExp should not be evaluated"
evalExp (MinusExp _ _) = fail "MinusExp should not be evalueted"
evalExp (MinusFloatExp _ _) = fail "MinusFloatExp should not be evaluated"
evalExp e = fail "not supported"

evalPat ::  (Pattern, Exp) -> State EvalState (Pattern, Exp)
evalPat (p, e) =
    do
      env <- get
      e' <- evalExp e
      put $ (p, e'):env
      return (p, e')
