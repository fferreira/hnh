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
module AddMetaTypes
       (
         addMetaTypes
       )
       where

import Syntax
import AddIdentifiers (idEnv0)
import BuiltIn (builtInDataTs)
import TransformMonad (TransformM, transformOk, transformError)
import TypeUtils(getType, getDataTypes, DataType, getConstTypeParams)

import Tools

import Control.Monad.State(evalState, State, put, get)

addMetaTypes :: Program -> TransformM Program
addMetaTypes p@(Program decls) = transformOk "addMetaTypes" (Program decls')
  where
    decls' = processDecls dataTs decls
    dataTs = builtInDataTs ++ getDataTypes p


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
       Nothing -> error ("xIdentifier " ++ (show i) ++ " not found in "++ show env)

processDecls :: [DataType] -> [Declaration] -> [Declaration]
processDecls dts decls =
  evalState (do mapM (processDecl dts) decls) initialSt
  
processDecl :: [DataType] -> Declaration -> State MetaSt Declaration  
processDecl dts d@(DataDcl t cons) =
  do mapM (typeCons t) cons
     return d
     
processDecl dts (PatBindDcl p e) = 
  do p' <- typePattern dts p
     e' <- typeExp dts e
     return $ PatBindDcl p' e'
     
processDecl dts (FunBindDcl _ _ _) = -- TODO Improve error handling
  error "Unexpected function declartion at this point"
processDecl _ d = return d

typeCons :: Type -> Constructor -> State MetaSt Constructor
typeCons t c@(IdConDcl i ts) =
  do MetaSt next env <- get
     put $ MetaSt next ((i, (toFun (ts ++ [t]))):env)
     return c
     where
       toFun (t:[]) = t
       toFun (t:ts) = FunType t (toFun ts)


typePattern :: [DataType] -> Pattern -> State MetaSt Pattern
typePattern _ (VarPat _ _) = error "Error Id??? pattern expected" 
typePattern _ (ConPat _ _ _) = error "Error Id??? pattern expected" 
typePattern _ (TuplePat _ _) = error "Error Id??? pattern expected" 
     
typePattern _ (WildcardPat UnknownType) =
  do t <- getMeta
     return $ WildcardPat t

typePattern _ (IdVarPat i UnknownType) =
  do t <- getMeta
     addMeta i t
     return $ IdVarPat i t     
typePattern dts (IdConPat n ids ts UnknownType) =
  do t <- getMeta 
     -- ts' <- getNMetas (length ids)
     -- ts' <- return $ (traceVal (getConsTypes (Program ds) n)) -- TODO Finish this
     ts' <- return $ case getConstTypeParams dts n of 
       Just res -> res
       Nothing -> error ("Error "++ n ++ " not found")
     addMetas ids ts' -- TODO validate length ids == length ts'
     return $ IdConPat n ids ts' t

typePattern _ (IdTuplePat ids UnknownType) =     
  do ts <- getNMetas (length ids)
     addMetas ids ts
     return $ IdTuplePat ids (TupleType ts)
     
typePattern _ p = return p

typeExp :: [DataType] -> Exp -> State MetaSt Exp
-- TODO improve error handling
typeExp _ (VarExp _ _) = error "Unexpected VarExp"
typeExp _ (ConExp _ _) = error "Unexpected ConExp"
typeExp _ (InfixOpExp _ _) = error "Unexpected InfixOpExp"
typeExp _ (MinusExp _ _) = error "Unexpected MinusExp"
typeExp _ (MinusFloatExp _ _) = error "Unexpected MinusFloatExp"
typeExp _ (IdVarExp i t) =
  do t' <- lookupId i
     return $ IdVarExp i (choose t t')

typeExp _ (IdConExp i t) =
  do t' <- lookupId i
     return $ IdConExp i (choose t t')

typeExp dts (FExp e1 e2 t) = 
  do e1' <- typeExp dts e1
     e2' <- typeExp dts e2
     t' <- getMeta
     return $ FExp e1' e2' (choose t t')
typeExp dts (LambdaExp ps e t) =
  do MetaSt _ env <- get
     ps' <- mapM (typePattern dts) ps
     e' <- typeExp dts e
     t' <- getMeta
     MetaSt num _ <- get
     put $ MetaSt num env
     return $ LambdaExp ps' e' (choose t t')
     
typeExp dts (LetExp decls e t) =
  do MetaSt _ env <- get 
     decls' <- mapM (processDecl dts) decls
     e' <- typeExp dts e
     t' <- getMeta
     MetaSt num _ <- get
     put $ MetaSt num env
     return $ LetExp decls' e' (choose t t')
     
typeExp dts (IfExp e1 e2 e3 t) =
  do e1' <- typeExp dts e1
     e2' <- typeExp dts e2
     e3' <- typeExp dts e3
     return $ IfExp e1' e2' e3' (choose t (getType e2'))

typeExp dts (CaseExp es alts t) = 
  do es' <- mapM (typeExp dts) es
     alts' <- mapM (typeAlt dts) alts
     t' <- getMeta
     return $ CaseExp es' alts' (choose t t')

typeExp dts (ParensExp e t) =
  do e' <- typeExp dts e
     return $ ParensExp e' (choose t (getType e'))
     
typeExp dts (TupleExp es t) =
  do es' <- mapM (typeExp dts) es
     return $ TupleExp es' (choose t (TupleType (map getType es')))
typeExp dts (ListExp es t) = 
  do es' <- mapM (typeExp dts) es
     t' <- getMeta
     return $ ListExp es' (choose t t')
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


