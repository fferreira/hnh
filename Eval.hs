module Eval where --TODO add exported methods

import Parser
import Text.PrettyPrint.Leijen
import Syntax

import ExprTransformer(correctPrecedence, toPrefix)
import TreeSimplify(funToLambda)

import qualified ParserMonad as P
import qualified TransformMonad as T
import ErrorMonad

import EvalEnv(Env,Value(..), lookupEvalEnv)

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
eval :: Program -> Name -> State EvalState Value
eval (Program declarations) name =
    do
      buildEvalEnv declarations
      env <- get
      (main, v) <- lookupEvalEnv name env
      case main of 
        (VarPat _ _) -> return v  
        _ -> fail (name ++ " not found or not the right from")

buildEvalEnv :: [Declaration] -> State EvalState [Env]
buildEvalEnv decls = mapM declToValue (filter isPatBind decls)
    where
      declToValue (PatBindDcl pat e) = 
          do
            env <- get
            v <- evalExp e
            put((pat,v):env)
            return (pat, v)
      isPatBind (PatBindDcl _ _) = True
      isPatBind _ = False

evalExp :: Exp -> State EvalState Value
evalExp e@(LitExp l _) = return (LitVal l)
evalExp e@(ConExp c t) = return (ConVal c)
evalExp (VarExp n _) = 
    do
      env <- get
      (p, v) <- lookupEvalEnv n env
      return v

evalExp (IfExp e1 e2 e3 _) =
    do
      e1' <- evalExp e1
      res <- case e1' of
               (ConVal "True" ) -> evalExp e2
               (ConVal "False") -> evalExp e3
               _ -> fail "if condition not boolean"
      return res

evalExp (LetExp decls e _) =
    do
      env <- get
      buildEvalEnv decls -- get env'

      v <- evalExp e
      put env -- restores the original environment
      return v

evalExp (LambdaExp pats e _) =
    do
      env <- get
      return $ Closure pats env e

evalExp (ParensExp e _) = evalExp e

evalExp (InfixOpExp _ _) = fail "InfixOpExp should not be evaluated"
evalExp (MinusExp _ _) = fail "MinusExp should not be evalueted"
evalExp (MinusFloatExp _ _) = fail "MinusFloatExp should not be evaluated"
evalExp e = fail "not supported"

evalPat ::  (Pattern, Exp) -> State EvalState (Pattern, Value)
evalPat (p, e) =
    do
      env <- get
      v <- evalExp e
      put $ (p, v):env
      return (p, v)
