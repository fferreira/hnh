module InferTypes
    (
     performTypeInference
    )
    where

import Syntax
import TransformMonad (TransformM, transformOk, transformError)
import AddMetaTypes(addMetaTypes)

{-
  Type inferences is done in 3 phases:
    1- add type variables to all unknown types 
    2- generate equations to represent the type of expressions (check for impossible types)
    3- unify all the types
-}

performTypeInference :: Program -> TransformM Program
performTypeInference p = addMetaTypes p

