module InferTypes
{-    (
     performTypeInference
    )-} --TODO debugging only remove
    where

import Syntax
import BuiltIn(EnvType, env0)
import TransformMonad (TransformM, transformOk, transformError)
import KnownTypes(declsToEn)
import TypeUtils(addType, getType)

import Control.Monad.State
import Data.List(nub)

import Text.PrettyPrint.Leijen -- requires wl-pprint installed (available in cabal)

import Debug.Trace
import Tools(traceVal)

performTypeInference :: Program -> TransformM Program
performTypeInference (Program decls) = return $ Program decls

{-performTypeInference (Program decls) = typedProgram --(typeExpressions env0 (addMetaTypes decls))
    where
      typedDecls = addMetaTypes decls
      typedProgram = case typeDeclarations (env0 ++ declsToEn typedDecls) typedDecls of
                       Nothing -> transformError "PerformTypeInference: error" --TODO Improve this
                       Just result -> transformOk "performTypeInference" $ Program result --TODO Use ErrorM
-}

{-
  Type inferences is done in 3 phases:
    1- add type variables to all unknown types (additionally, convert vars to idents)
    2- generate equations to represent the type of expressions (check for impossible types)
    3- unify all the types
-}





