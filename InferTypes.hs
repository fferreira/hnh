module InferTypes
    (
     performTypeInference
    )
    where

import Syntax
import AddMetaTypes(addMetaTypes)
import GenerateConstraints(generateConstraints)
import UnifyTypes(unifyTypes, Subst)
import TypeUtils(addType, getType, addPatType, getPatType)
import TransformMonad (TransformM, transformOk, transformError)
import TransformUtils(transformTree, Transformer(..), defTrans)
import ErrorMonad(ErrorM(..))

-- import Tools

{-
  Type inferences is done in 3 phases:
    1- add type variables to all unknown types (adMetaTypes)
    2- generate equations to represent the type of expressions (generateConstraints)
    3- unify all the types (unifyTypes)
    4- perform the replacements indicated by the unification
-}

performTypeInference :: Program -> TransformM Program
performTypeInference p = addMetaTypes p
                         >>= performSubstitutions

performSubstitutions :: Program -> TransformM Program
performSubstitutions prog = transformTree
                            "performSubstitutions"
                            defTrans { tExp = adaptExp subs, tPat = adaptPat subs }
                            prog
                              where
                                constraints =  generateConstraints prog
                                subs = case unifyTypes constraints of
                                  Success ss -> ss
                                  Error msg -> error msg --TODO improve error handling
                                  

adaptExp :: [Subst] -> Exp -> ErrorM Exp
adaptExp subs e  = return $ addType e (rep subs (getType e))

adaptPat :: [Subst] -> Pattern -> ErrorM Pattern
adaptPat subs p = return $ addPatType p (rep subs (getPatType p))
                                  
rep :: [Subst] -> Type -> Type
rep ((t1, t2):subs) t = if t == t2 then t1 else rep subs t
rep [] t = t
                            