{-
  This file is part of HNH.

    HNH is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    HNH is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Foobar.  If not, see <http://www.gnu.org/licenses/>.
    
    Copyright 2010 Francisco Ferreira
-}

module Eval where --TODO add exported methods

import Parser
import Text.PrettyPrint.Leijen
import Syntax

import CommonTransforms(commonTransforms)

import qualified TransformMonad as T
import ErrorMonad

import EvalEnv(Env, env0, Value(..), ClosureAction(..), lookupEvalEnv, envForData)

import Data.List(intersperse)
import Control.Monad.State

import Tools
import Debug.Trace


evaluationTransform :: Program -> (ErrorM Program, [(String, Doc)])
evaluationTransform p = 
    let (res, docs)  = T.runTransform (commonTransforms p
                                       >>= return)       
    in
      (res, ("original", (pretty p)):docs) -- adding the original to the list

runTransformations :: ErrorM Program -> (ErrorM Program, [(String, Doc)])
runTransformations (Success p) = evaluationTransform p
runTransformations (Error s) = error $ show (pretty s)

checkTransformation :: ErrorM Program -> Program
checkTransformation (Success program) = program
checkTransformation (Error err) = error err

loadAndEval :: String -> Name -> Bool -> IO Doc
loadAndEval file main showSteps = do contents <- readFile file
                                     preludeContents <- readFile "prelude.hnh"
                                     parsedPrelude <- return $ parseHNH "prelude.hnh" preludeContents
                                     parsed <- return $ parseHNH file contents
                                     (programRes, docs) <- return $ runTransformations (merge parsedPrelude parsed)
                                     program <- return $ checkTransformation programRes 
                                     doc <- return $ if showSteps then
                                                         T.renderSteps docs
                                                     else
                                                         pretty $ eval program main
                                     return doc

merge :: ErrorM Program -> ErrorM Program -> ErrorM Program
merge (Success (Program d1)) (Success (Program d2)) = Success (Program (d1++d2))
merge e@(Error msg) _ = e
merge _ e@(Error msg) = e

eval :: Program -> Name -> Value
eval (Program decls) name =
    evalState (do
                env <- buildEvalEnv decls
                put env
                (main, v) <- lookupEvalEnv name env
                case main of 
                  (VarPat _ _) -> return v  
                  _ -> fail (name ++ " not found or not the right from")) (env0++dataTypes decls)

dataTypes :: [Declaration] -> [Env]
dataTypes ((DataDcl (DataType n _)  cons):ds) = ((envForData n cons)++(dataTypes ds))
dataTypes (d:ds) = dataTypes ds
dataTypes [] = []


buildEvalEnv :: [Declaration] -> State [Env] [Env]
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

evalExp :: Exp -> State [Env] Value
evalExp (ParensExp e _) = evalExp e
evalExp e@(LitExp (LiteralInt v) _) = return (IntVal v)
evalExp e@(LitExp (LiteralFloat v) _) = return (FloatVal v)
evalExp e@(LitExp (LiteralString v) _) = return (StringVal v)
evalExp e@(LitExp (LiteralChar v) _) = return (CharVal v)
evalExp e@(ConExp c t) =
    do
      env <- get
      (p, v) <- lookupEvalEnv c env
      return v

evalExp (VarExp n _) = 
    do
      env <- get
      (p, v) <- lookupEvalEnv n env 
      return v

evalExp (IfExp e1 e2 e3 _) =
    do
      v1 <- evalExp e1
      res <- case v1 of
               (ConVal "True" _) -> evalExp e2
               (ConVal "False" _) -> evalExp e3
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
      return $ Closure pats env (CExp e)

evalExp (FExp e1 e2 _) =
    do
      v1 <- evalExp e1 -- it should evaluate to a closure
      v2 <- evalExp e2
      applyFun v1 v2

evalExp (CaseExp es alts _) = 
    do
      env <- get
      vs <- mapM evalExp es
      res <- findAlternative  vs  alts
      put env
      return res

evalExp (TupleExp exps _) =
    do
      vs <- mapM evalExp exps
      return $ TupleVal vs

evalExp (ListExp exps _) =
    do
      vs <- mapM evalExp exps
      return $ listToVal vs

evalExp (InfixOpExp _ _) = fail "InfixOpExp should not be evaluated"
evalExp (MinusExp _ _) = fail "MinusExp should not be evalueted"
evalExp (MinusFloatExp _ _) = fail "MinusFloatExp should not be evaluated"

evalPat ::  (Pattern, Exp) -> State [Env] (Pattern, Value)
evalPat (p, e) =
    do
      env <- get
      v <- evalExp e
      put $ (p, v):env
      return (p, v)

applyFun :: Value -> Value -> State [Env] Value
applyFun (Closure (p:[]) env e) v = evalClosure (Closure [] ((p, v):env)  e)
applyFun (Closure (p:ps) env e) v = return (Closure ps ((p, v):env) e)

applyFun v1 _ = fail "Apply function requires a closure"

evalClosure :: Value -> State [Env] Value
evalClosure (Closure [] env (CExp e)) =
    do
      env' <- get
      put (env++env')
      v <- evalExp e
      put env'
      return v

evalClosure (Closure [] env (CFun f)) = return $ f env
evalClosure c = fail "evalClosure needs a closure to evaluate"

findAlternative :: [Value] -> [Alternative] -> State [Env] Value
findAlternative vs ((Alternative pats e):alts) = 
    do
      current <- checkAlternatives vs pats
      if current then
          evalExp e
        else
          findAlternative vs alts
findAlternative vs [] = fail "Exhausted alternatives!"

checkAlternatives :: [Value] -> [Pattern] -> State [Env] Bool
checkAlternatives vs pats =
    do
      env <- get
      matches <- mapM (\(v,p) -> checkAlternative v p) (zip vs pats)
      if (foldl (&&) True matches) == True then
          return True
       else
           do
             put env
             return False

checkAlternative :: Value -> Pattern -> State [Env] Bool
checkAlternative v (WildcardPat _) = return True
checkAlternative v p@(VarPat _ _) =
    do
      env <- get
      put ((p,v):env)
      return True

checkAlternative (TupleVal vs) p@(TuplePat ns _) =
    if length vs == length ns then
        do
          env <- get
          put $ (zip (map (\n->VarPat n UnknownType) ns) vs) ++ env
          return True
     else return False

checkAlternative v (TuplePat _ _) = return False

checkAlternative (ConVal n1 vs) (ConPat n2 ns _) =
    if (n1==n2)&&(length vs == length ns) then
        do
          env <- get
          put $ (zip (map (\n->VarPat n UnknownType) ns) vs) ++ env
          return True
    else return False
checkAlternative v (ConPat n2 ns _) = return False

listToVal :: [Value] -> Value -- converts a haskell list to hnh builtin list
listToVal (v:vs) = (ConVal "Cons" ([v]++[listToVal vs]))
listToVal [] = (ConVal "Nil" [])