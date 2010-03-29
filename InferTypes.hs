module InferTypes
    (
     performTypeInference
    )
    where

import Syntax
import TransformMonad(TransformM, transformOk)
import TypeUtils(DataType, getDataTypes)
import AddIdentifiers(idEnv0)
import InferDeclaration(inferDeclType)

import Control.Monad.State(evalState)

{-
  Type inferences is done in these phases:
    1- add type variables to all unknown types (adMetaTypes)
    2- generate equations to represent the type of expressions (generateConstraints)
    3- unify all the types (unifyTypes)
    4- perform the replacements indicated by the unification
    5- generalize the free types
-}

performTypeInference :: Program -> TransformM Program
performTypeInference p = inferTypesProg dts p
  where
    dts = getDataTypes p

inferTypesProg :: [DataType] -> Program -> TransformM Program
inferTypesProg dts (Program decls) = 
  transformOk "inferTypes" (Program 
                            (evalState (mapM (inferDeclType dts) decls) idEnv0))



