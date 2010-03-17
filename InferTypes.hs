module InferTypes
{-    (
     performTypeInference
    )-} --TODO debugging only remove
    where

import Syntax
import AddIdentifiers (idEnv0)
-- import BuiltIn(EnvType, env0)
import TransformMonad (TransformM, transformOk, transformError)
-- import KnownTypes(declsToEn)
-- import TypeUtils(addType, getType)

import Control.Monad.State(evalState, State, put, get)
import Data.List(nub)

import Text.PrettyPrint.Leijen -- requires wl-pprint installed (available in cabal)

import Tools(traceVal)

performTypeInference :: Program -> TransformM Program
performTypeInference p = addMetaTypes p

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


addMetaTypes :: Program -> TransformM Program
addMetaTypes (Program decls) = transformOk "addMetaTypes" (Program decls')
  where
    decls' = processDecls decls


data MetaSt = MetaSt {
  nextNum :: Int
  , env :: [(Identifier, Type)] 
  }

initialSt = MetaSt 0 idEnv0

getMeta :: State MetaSt Type
getMeta =
  do MetaSt next env <- get
     put $ MetaSt (next + 1) env
     return $ MetaType next
     
getNMetas :: Int -> State MetaSt [Type]     
getNMetas 0 = return []
getNMetas n = do t <- getMeta ; ts <- getNMetas (n-1) ; return (t:ts)

addMeta :: Identifier -> Type -> State MetaSt ()     
addMeta i t =
  do MetaSt num env <- get
     put $ MetaSt num ((i, t):env)
     
addMetas :: [Identifier] -> [Type] -> State MetaSt ()     
addMetas ids ts =
  do MetaSt num env <- get
     put $ MetaSt num ((zip ids ts) ++ env)
     
lookupId :: Identifier -> State MetaSt Type     
lookupId i =
  do MetaSt _ env <- get
     case lookup i env of
       Just i -> return i
       Nothing -> error ("Identifier " ++ (show i) ++ " not found")

processDecls :: [Declaration] -> [Declaration]
processDecls decls =
  evalState (do mapM processDecl decls) initialSt
  
processDecl :: Declaration -> State MetaSt Declaration  
processDecl (PatBindDcl p e) = 
  do p' <- typePattern p
     e' <- typeExp e
     return $ PatBindDcl p' e'
     
processDecl (FunBindDcl _ _ _) = -- TODO Improve error handling
  error "Unexpected function declartion at this point"
processDecl d = return d

typePattern :: Pattern -> State MetaSt Pattern
typePattern (VarPat _ _) = error "Error Id??? pattern expected" 
typePattern (ConPat _ _ _) = error "Error Id??? pattern expected" 
typePattern (TuplePat _ _) = error "Error Id??? pattern expected" 
     
typePattern (WildcardPat UnknownType) =
  do t <- getMeta
     return $ WildcardPat t

typePattern (IdVarPat i UnknownType) =
  do t <- getMeta
     addMeta i t
     return $ IdVarPat i t     
typePattern (IdConPat n ids UnknownType) =
  do t <- getMeta --TODO add the right type here!
     return $ IdConPat n ids t
typePattern (IdTuplePat ids UnknownType) =     
  do ts <- getNMetas (length ids)
     addMetas ids ts
     return $ IdTuplePat ids (TupleType ts)
     
typePattern p = return p

typeExp :: Exp -> State MetaSt Exp
-- TODO improve error handling
typeExp (VarExp _ _) = error "Unexpected VarExp"
typeExp (ConExp _ _) = error "Unexpected ConExp"
typeExp (InfixOpExp _ _) = error "Unexpected InfixOpExp"
typeExp (MinusExp _ _) = error "Unexpected MinusExp"
typeExp (MinusFloatExp _ _) = error "Unexpected MinusFloatExp"
typeExp (IdVarExp i UnknownType) =
  do t <- lookupId i
     return $ IdVarExp i t

typeExp (IdConExp i UnknownType) =
  do t <- lookupId i
     return $ IdConExp i t

-- typeExp (FExp _ _ _) = undefined
-- typeExp (LambdaExp _ _ _) = undefined
-- typeExp (LetExp _ _ _) = undefined
-- typeExp (IfExp _ _ _ _) = undefined
-- typeExp (CaseExp _ _ _) = undefined
-- typeExp (ParensExp _ _) = undefined
-- typeExp (TupleExp _ _) = undefined
-- typeExp (ListExp _ _) = undefined
typeExp e = return e