module InferTypes
{-    (
     performTypeInference
    )-} --TODO debugging only remove
    where

import Syntax
import BuiltIn(Env, env0)
import TransformMonad (TransformM, transformOk)

import Control.Monad.State

performTypeInference :: Program -> TransformM Program
performTypeInference (Program decls) = transformOk $ Program (addMetaTypes decls)
-- TODO add the patterns in lambdas and declarations in lets
----------------------------------------------------

type MTState = Int -- meta type state


addMetaTypes :: [Declaration] -> [Declaration]
addMetaTypes decls = evalState (addMetaTypes' decls) 0


addMetaTypes' :: [Declaration] -> State MTState [Declaration]
addMetaTypes' ((FunBindDcl n p r t):ds) = 
    do
      p' <- mapM numberPattern p
      ds' <- addMetaTypes' ds
      r' <- addToRhs r
      return ((FunBindDcl n p' r' t) :ds')

addMetaTypes' ((PatBindDcl p r):ds) =
    do
      p' <- numberPattern p
      ds' <- addMetaTypes' ds
      r' <- addToRhs r
      return ((PatBindDcl p' r'):ds')

addMetaTypes' (d:ds) =
    do
      ds' <- addMetaTypes' ds
      return (d:ds')
addMetaTypes' [] = return []

addToRhs :: Rhs -> State MTState Rhs
addToRhs (UnGuardedRhs e) = 
    do
      e' <- addToExp e
      return (UnGuardedRhs e')

addToRhs (GuardedRhs guards) =
    do
      guards' <- mapM applyToGuard guards
      return (GuardedRhs guards')
    where
      applyToGuard (Guard e1 e2) =
          do
            e1' <- addToExp e1
            e2' <- addToExp e2
            return (Guard e1' e2')

addToExp :: Exp -> State MTState Exp
addToExp (FExp e1 e2 t) =
    do
      e1' <- addToExp e1
      e2' <- addToExp e2
      return (FExp e1' e2' t)

addToExp (LambdaExp pats e t) = 
    do
      pats' <- mapM numberPattern pats
      e' <- addToExp e
      return (LambdaExp pats' e' t)

addToExp (LetExp decls e t) =
    do 
      decls' <- addMetaTypes' decls
      e' <- addToExp e
      return (LetExp decls' e' t)

addToExp (IfExp e1 e2 e3 t) =
    do
      e1' <- addToExp e1
      e2' <- addToExp e2
      e3' <- addToExp e3
      return (IfExp e1' e2' e3' t)

addToExp (CaseExp e alts t) =
    do
      e' <- addToExp e
      alts' <- mapM addToAlt alts
      return (CaseExp e' alts' t)

addToExp (ParensExp e t) =
    do
      e' <- addToExp e
      return (ParensExp e t)

addToExp (TupleExp es t) =
    do
      es' <- mapM addToExp es
      return (TupleExp es' t)

addToExp (ListExp es t) =
    do
      es' <- mapM addToExp es
      return (ListExp es' t)

addToExp e = return e

addToAlt :: Alternative -> State MTState Alternative
addToAlt (Alternative p e) =
    do
      p' <- numberPattern p
      e' <- addToExp e
      return (Alternative p' e')

-- numberPattern: adds metatypes to the pattern UnknownType
numberPattern :: Pattern -> State MTState Pattern
numberPattern (VarPat n UnknownType) = 
    do
      s <- get
      put (s+1)
      return (VarPat n (MetaType s))

numberPattern (ConPat n ns UnknownType)  =
    do
      s <- get
      put (s+1)
      return  (ConPat n ns (MetaType s))

numberPattern (HeadTailPat n1 n2 UnknownType) = 
    do
      s <- get
      put (s+1)
      return (HeadTailPat n1 n2 (MetaType s)) --TODO really this is a list of meta 

numberPattern (TuplePat ns _) =
    do
      ts <- numberVarList (length ns)
      return (TuplePat ns (TupleType ts))

numberPattern (WildcardPat UnknownType) = 
    do
      s <- get
      put (s+1)
      return (WildcardPat (MetaType s))
numberPattern p = return p -- TODO debug remove

-- numberVarList: returns a list of metatypes starting from the first available
numberVarList :: Int -> State MTState [Type]
numberVarList i =
    do
      s <- get
      put (s + i)
      return $ map (\i -> MetaType i) [s..(s+i-1)]

{- agregar los metatypes con State monad, solo para las declaraciones de variables (done)
   agregar typos a los valores segun scope
   ver como carajo unificar todo ese bolonqui
-}

--------------------------------------------------
{- 
findDeclaration :: Monad m => Name -> [Declaration] -> m Declaration
findDeclaration n decls = 
    case filter (\d -> elem n (namesDeclared d)) decls of
      x:[] -> return x
      _ -> fail "not found, or multiple declarations"

namesDeclared :: Declaration -> [Name]
namesDeclared (FunBindDcl n _ _ _) = [n]
namesDeclared (PatBindDcl p _) = namesFromPattern p

namesFromPattern :: Pattern -> [Name]
namesFromPattern (VarPat n _) = [n]
namesFromPattern (ConPat _ ns _) = ns
namesFromPattern (HeadTailPat n1 n2 _) = [n1, n2]
namesFromPattern (TuplePat ns _) = ns
namesFromPattern (WildcardPat _) = []

getRhsType :: Rhs -> Type
getRhsType = undefined

-}
{-
infer env (FunBindDcl n pats r) = undefined
infer env (PatBindDcl pat r) = undefined
infer env d = undefined
-}