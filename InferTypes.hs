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
performTypeInference (Program decls) = typedProgram --(typeExpressions env0 (addMetaTypes decls))
    where
      typedDecls = addMetaTypes decls
      typedProgram = case typeDeclarations (env0 ++ declsToEn typedDecls) typedDecls of
                       Nothing -> transformError "PerformTypeInference: error" --TODO Improve this
                       Just result -> transformOk "performTypeInference" $ Program result --TODO Use ErrorM

----------------------------------------------------

type MTState = Int -- meta type state

{-
addMetaTypes:

First phase of inference, it will add meta type to all the variable declarations in the program
-}

addMetaTypes :: [Declaration] -> [Declaration]
addMetaTypes decls = evalState (addMetaTypes' decls) 0


addMetaTypes' :: [Declaration] -> State MTState [Declaration]
addMetaTypes' ((FunBindDcl n p e t):ds) = 
    do
      s <- get
      put (s+1)
      p' <- mapM numberPattern p
      ds' <- addMetaTypes' ds
      e' <- addToExp e
      return ((FunBindDcl n p' e' (MetaType s)) :ds')

addMetaTypes' ((PatBindDcl p e):ds) =
    do
      p' <- numberPattern p
      ds' <- addMetaTypes' ds
      e' <- addToExp e
      return ((PatBindDcl p' e'):ds')

addMetaTypes' (d:ds) =
    do
      ds' <- addMetaTypes' ds
      return (d:ds')
addMetaTypes' [] = return []

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

addToExp (CaseExp es alts t) =
    do
      es' <- mapM addToExp es
      alts' <- mapM addToAlt alts
      return (CaseExp es' alts' t)

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
addToAlt (Alternative ps e) =
    do
      ps' <- mapM numberPattern ps
      e' <- addToExp e
      return (Alternative ps' e')

-- numberPattern: adds metatypes to the pattern UnknownType
numberPattern :: Pattern -> State MTState Pattern
numberPattern (VarPat n UnknownType) = 
    do
      s <- get
      put (s+1)
      return (VarPat n (MetaType s))

numberPattern (ConPat n ns UnknownType)  =
    do
      t <- numberVarList (length ns)
      return  (ConPat n ns (ConType n t))

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

-- this will type the functions according to the meta variables, or it will fail 
-- if the types are irrecocilable
typeDeclarations :: Monad m => [EnvType] -> [Declaration] -> m [Declaration]
typeDeclarations env decls = 
    do
      (mapM (typeDeclaration env') decls)
    where
      env' = env ++ getEnv decls

typeDeclaration :: Monad m => [EnvType] -> Declaration -> m Declaration
typeDeclaration env (FunBindDcl n pats e t) =
    do
      e' <- typeExp env e
      return (FunBindDcl n pats e' t)

typeDeclaration env (PatBindDcl p e) =
    do
      e' <- typeExp env e
      return (PatBindDcl p e')
typeDeclaration env decl = return decl

typeExp :: Monad m => [EnvType] -> Exp -> m Exp
typeExp env (VarExp n t) = 
    do
      tenv <- lookupEnv n env
      tres <- checkType t tenv 
      return (VarExp n (traceVal tres))

typeExp env (ConExp n t) =
    do
      tenv <- lookupEnv n env
      tres <- checkType t tenv 
      return (ConExp n (traceVal tres))

typeExp env (IfExp e1 e2 e3 t) =
    do
      e1' <- typeExp env e1
      e2' <- typeExp env e2
      e3' <- typeExp env e3
      tbool <- checkType (getType e1') (ConType "Bool" []) -- e1 hast to be a boolean expression
      tes <- checkType (getType e2') (getType e3') -- e2 and e3 have to share the type
      tres <- checkType tes t -- the if exp has to have the same type as e2, e3
      return (IfExp (addType e1' (ConType "Bool" [])) (addType e2' tres) (addType e3' tres) tres)

typeExp env (ParensExp e t) =
    do
      e' <- typeExp env e
      t' <- checkType (getType e') t
      return $ ParensExp e' t'

typeExp env (TupleExp es t) =
    do
      es' <- mapM (typeExp env) es
      t' <- checkType (TupleType (map getType es')) t
      return (TupleExp es' t')

{-
typeExp env (ListExp es t) =
    do
      es' <- mapM (typeExp env) es
      t' <- checkType (ConType "List" [UnknownType]) t
      tmatch <- mapM (\s -> checkType s (getListParam t')
-}


{-
typeExp env (ListExp es t) = undefined

typeExp env (CaseExp e alts t) = undefined

-}

{- Complicated expression follow :P
typeExp env (FExp Exp Exp Type 

typeExp env (LambdaExp [Pattern] Exp Type
typeExp env (LetExp [Declaration] Exp Type
-}

typeExp env e = return e -- TODO this is for literals and something else? replace by LitExp

checkType :: Monad m => Type -> Type -> m Type
checkType t1 t2 = checkType' (traceVal t1) (traceVal t2)

checkType' :: Monad m => Type -> Type -> m Type
checkType' (FuncType t1 t2) (FuncType t3 t4) =
    do
      t1' <- checkType t1 t2
      t2' <- checkType t3 t4
      return $ FuncType t1' t2'

checkType' (TupleType t1s) (TupleType t2s) =
    do
      t' <- mapM (\(t1,t2) -> checkType t1 t2) (zip t1s t2s)
      if  (length t1s == length t2s)   then
                   return (TupleType t')
               else
                   fail "non compatible tuples"

checkType' (ConType n1 l1) (ConType n2 l2) =
    do
      tpar <- mapM (\(t1, t2) -> checkType t1 t2) (zip l1 l2)

      if (n1 == n2) then
          return (ConType n1 tpar)
       else
          fail "non compatible types"


checkType' t@(MetaType _) _ = return t --TODO concrete types shouldn't win?
checkType' _ t@(MetaType _) = return t
checkType' _ t@(VarType _) = return t  -- TODO this has to be changed from expression to expression
checkType' t@(VarType _) _ = return t

checkType' UnknownType UnknownType = fail "Unconcilable checkType UnknownType UnkownType"
checkType' t UnknownType = return t
checkType' UnknownType t = return t
checkType' t1 t2 = if t1 == t2 then return t2 else fail(show $ pretty "type" <+> pretty t1 
                                                              <+> pretty "not compatible with" <+> pretty t2)

lookupEnv :: Monad m => Name -> [EnvType] -> m Type
lookupEnv n e = 
    do
      v <- lookupEnv' n e 
      return $ traceVal (v)

-- monadic lookup from the environment
lookupEnv' :: Monad m => Name -> [EnvType] -> m Type
lookupEnv' n env =
    do
      case lookup n (traceVal env) of
        Nothing -> fail ("fundefined " ++ n)
        Just t -> return t

-- getEnv: gets an environment with all the names declared by a list of declarations
getEnv :: [Declaration] -> [EnvType]
getEnv decls = validate $ concatMap namesDeclared decls

    where
      validate env = 
          let 
              (vars, _) = unzip env
          in
            if (length vars) == (length $ nub vars) then -- there should not be duplicated entries
                env
            else
                error "Duplicated variables!" --TODO add proper error handling


namesDeclared :: Declaration -> [EnvType]
namesDeclared (FunBindDcl n _ _ t) = [(n, t)]  
namesDeclared (PatBindDcl p _) = namesFromPattern p
    where
      namesFromPattern :: Pattern -> [EnvType]
      namesFromPattern (VarPat n t) = [(n, t)]

      namesFromPattern (ConPat _ ns (ConType _ ts)) = zip ns ts

      namesFromPattern (TuplePat ns (TupleType ts)) = zip ns ts
      namesFromPattern (WildcardPat _) = []

namesDeclared _ = []

{-
infer env (FunBindDcl n pats r) = undefined
infer env (PatBindDcl pat r) = undefined
infer env d = undefined
-}

