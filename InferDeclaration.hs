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
    along with HNH.  If not, see <http://www.gnu.org/licenses/>.
    
    Copyright 2010 Francisco Ferreira
-}
module InferDeclaration
       (
         inferDeclType
       )
       where

import Syntax
import TypeUtils(getType , DataType, getConstTypeParams, getConstType)
import PolyType(transformType, initialPoly, getNext)
import GenerateConstraints(getConstraints)
import GeneralizeTypes(generalizeTypes)
import UnifyTypes(unifyTypes)
import Substitutions(replaceInDecl)
import ErrorMonad(ErrorM(..))

import Control.Monad.State(runState, State, put, get)

inferDeclType :: [DataType] -> Declaration -> State [(Identifier, Type)] Declaration
inferDeclType dts d = 
  do env <- get
     (metaD, env') <- return $ addDeclMeta dts env d
     d' <- return $ inferFromMeta metaD
     put $ addDeclToEnv d' env'
     return d'
     
inferFromMeta :: Declaration -> Declaration     
inferFromMeta d = case unifyTypes (getConstraints d) of
  Success subs -> generalizeTypes (replaceInDecl subs d)
  Error msg -> error msg

addDeclsToEnv (d:ds) env = addDeclsToEnv ds (addDeclToEnv d env)
addDeclsToEnv [] env = env

addDeclToEnv :: Declaration -> [(Identifier, Type)] -> [(Identifier, Type)]
addDeclToEnv (PatBindDcl (IdVarPat i t) _) env = (i,t):env
addDeclToEnv _ env = env

addDeclMeta ::  [DataType] 
                -> [(Identifier, Type)] 
                -> Declaration 
                -> (Declaration, [(Identifier, Type)])
addDeclMeta dts env d = 
  let (d', (MetaSt _ env')) = runState (processDecl dts d) (MetaSt 0 env)
  in (d', env')

data MetaSt = MetaSt Int [(Identifier, Type)]

--- State Manipulation functions

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
       Just t -> return t
       Nothing -> error ("Identifier " ++ (show i) 
                         ++ " not found in "++ show env)

--- Tree transformation functions

processDecl :: [DataType] -> Declaration -> State MetaSt Declaration  
-- processDecl dts d@(DataDcl t cons) =
  -- do mapM (typeCons t) cons
     -- return d
     
processDecl dts (PatBindDcl p e) = 
  do p' <- typePattern dts p
     e' <- typeExp dts e
     return $ PatBindDcl p' e'
     
processDecl dts (FunBindDcl _ _ _) = -- TODO Improve error handling
  error "Unexpected function declartion at this point"
processDecl _ d = return d

-- typeCons :: Type -> Constructor -> State MetaSt Constructor
-- typeCons t c@(IdConDcl i ts) =
--   do MetaSt next env <- get
--      put $ MetaSt next ((i, (toFun (ts ++ [t]))):env)
--      return c
--      where
--        toFun (t:[]) = t
--        toFun (t:ts) = FunType t (toFun ts)

typePattern :: [DataType] -> Pattern -> State MetaSt Pattern
typePattern _ (VarPat _ _) = error "Error Id??? pattern expected" 
typePattern _ (ConPat _ _ _) = error "Error Id??? pattern expected" 
typePattern _ (TuplePat _ _) = error "Error Id??? pattern expected" 
     
typePattern _ (WildcardPat t) =
  do t' <- polyType t
     return $ WildcardPat t'

typePattern _ (IdVarPat i t) =
  do t' <- polyType t
     addMeta i t'
     return $ IdVarPat i t'
     
typePattern dts (IdConPat n ids ts t) =
  do t' <- case getConstType dts n of Just tc -> return tc
                                      Nothing -> error (n ++ " not found")
     ts' <- return $ case getConstTypeParams dts n of 
       Just res -> res
       Nothing -> error ("Error "++ n ++ " not found")
     (t'', ts'') <- polyTypePair ([t'], ts')  
     addMetas ids ts'' -- TODO validate length ids == length ts'
     return $ IdConPat n ids ts' (head t'')

typePattern _ (IdTuplePat ids t) =     
  do ts' <- case t of (TupleType ts) -> if (length ids /= length ts) 
                                        then getNMetas (length ids)
                                        else polyTypes ts
                      _ -> getNMetas (length ids)
     addMetas ids ts'
     return $ IdTuplePat ids (TupleType ts')
     

typeExp :: [DataType] -> Exp -> State MetaSt Exp
-- TODO improve error handling
typeExp _ (VarExp _ _) = error "Unexpected VarExp"
typeExp _ (ConExp _ _ _) = error "Unexpected ConExp"
typeExp _ (InfixOpExp _ _) = error "Unexpected InfixOpExp"
typeExp _ (MinusExp _ _) = error "Unexpected MinusExp"
typeExp _ (MinusFloatExp _ _) = error "Unexpected MinusFloatExp"
typeExp _ (IdVarExp i t) =
  do tl <- lookupId i
     t' <- polyType (choose t tl)
     return (IdVarExp i t')

-- typeExp _ (IdConExp i params t) =
--   do tl <- lookupId i
--      t' <- polyType (choose t tl)
--      return $ IdConExp i params t'

typeExp dts (FExp e1 e2 t) = 
  do e1' <- typeExp dts e1
     e2' <- typeExp dts e2
     tl <- getMeta
     t' <- polyType (choose t tl)
     return $ FExp e1' e2' t'
typeExp dts (LambdaExp ps e t) =
  do MetaSt _ env <- get
     ps' <- mapM (typePattern dts) ps
     e' <- typeExp dts e
     tl <- getMeta
     MetaSt num _ <- get
     put $ MetaSt num env
     t' <- polyType (choose t tl)
     return $ LambdaExp ps' e' t'
     
typeExp dts (LetExp decls e t) =
  do MetaSt _ env <- get 
     decls' <- mapM (processDecl dts) decls
     st@(MetaSt n _) <- get
     put (MetaSt n (addDeclsToEnv (map inferFromMeta decls') env))
     e' <- typeExp dts e
     tl <- getMeta
     MetaSt num _ <- get
     put $ MetaSt num env
     t' <- polyType (choose t tl)
     return $ LetExp (map inferFromMeta decls') e' t'
     
typeExp dts (IfExp e1 e2 e3 t) =
  do e1' <- typeExp dts e1
     e2' <- typeExp dts e2
     e3' <- typeExp dts e3
     t' <- polyType (choose t (getType e2'))
     return $ IfExp e1' e2' e3' t'

typeExp dts (CaseExp es alts t) = 
  do es' <- mapM (typeExp dts) es
     alts' <- mapM (typeAlt dts) alts
     tl <- getMeta
     t' <- polyType (choose t tl)
     return $ CaseExp es' alts' t'

typeExp dts (ParensExp e t) =
  do e' <- typeExp dts e
     t' <- polyType (choose t (getType e'))
     return $ ParensExp e' t'
     
typeExp dts (TupleExp es t) =
  do es' <- mapM (typeExp dts) es
     t' <- polyType (choose t (TupleType (map getType es')))
     return $ TupleExp es' t'
typeExp dts (ListExp es t) = 
  do es' <- mapM (typeExp dts) es
     tl <- getMeta
     t' <- polyType (choose t tl)
     return $ ListExp es' t'
typeExp _ e = return e

typeAlt dts (Alternative ps e) =
  do MetaSt _ env <- get
     ps' <- mapM (typePattern dts) ps
     e' <- typeExp dts e
     MetaSt num _ <- get
     put $ MetaSt num env
     return $ Alternative ps' e'

choose :: Type -> Type -> Type
choose UnknownType t = t
choose t _ = t

--- Polymorphic Type Transformation
{-       
  This type transformation will replace all VarTypes
  by MetaTypes using it's own environment to replace
  the same variable by the same meta
-}

polyType :: Type -> State MetaSt Type
polyType t = 
  do MetaSt next env <- get
     (t,s) <- return $ runState (transformType t) (initialPoly next)
     put $ MetaSt (getNext s)  env
     return t

polyTypes :: [Type] -> State MetaSt [Type]     
polyTypes ts = 
  do MetaSt next env <- get
     (ts', s) <- return $ runState (mapM transformType ts) (initialPoly next)
     put $ MetaSt (getNext s) env
     return ts';
     
polyTypePair :: ([Type], [Type]) -> State MetaSt ([Type], [Type])     
polyTypePair (ta, tb) =
  do MetaSt next env <- get
     (tab, s) <- return $ runState (do ta' <- mapM transformType ta
                                       tb' <- mapM transformType tb
                                       return (ta', tb')
                                   ) (initialPoly next)
     put $ MetaSt (getNext s) env
     return tab;     
     
