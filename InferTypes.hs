module InferTypes
    (
     performTypeInference
    )
    where

import Syntax
-- import AddMetaTypes(addMetaTypes)
-- import Substitutions(performSubstitutions)
import TransformMonad(TransformM, transformOk)
-- import TransformUtils (transformTree, Transformer(..), defTrans)
import TypeUtils(DataType, getDataTypes)

import AddIdentifiers(idEnv0)
import AddMetaTypes(declarationMeta)
import GenerateConstraints(declarationConstraints)
import UnifyTypes(unifyTypes)
import Substitutions(replaceInDecl)
import GeneralizeTypes(generalizeTypes)
import ErrorMonad(ErrorM(..))
import Control.Monad.State(evalState, runState, State, put, get)


import Tools
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

inferDeclType :: [DataType] -> Declaration -> State [(Identifier, Type)] Declaration
inferDeclType dts d = 
  do env <- get
     (metaD, env') <- return $ declarationMeta dts env d
     constraints <- return $ declarationConstraints metaD
     subs <- case unifyTypes constraints of Success subs -> return subs
                                            --TODO improve error handling
                                            Error msg -> error msg
     d' <- return (replaceInDecl subs metaD)
     
     d'' <- return $ generalizeTypes d'
     put $ add d'' env'
     return d''

add :: Declaration -> [(Identifier, Type)] -> [(Identifier, Type)]
add (PatBindDcl (IdVarPat i t) _) env = (i,t):env
add _ env = env