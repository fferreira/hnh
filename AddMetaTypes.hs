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
import TransformMonad (TransformM, transformOk, transformError)
import TypeUtils(getType)

import Control.Monad.State(evalState, State, put, get)

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
       Nothing -> error ("Identifier " ++ (show i) ++ " not found in "++ show env)

processDecls :: [Declaration] -> [Declaration]
processDecls decls =
  evalState (do mapM processDecl decls) initialSt
  
processDecl :: Declaration -> State MetaSt Declaration  
processDecl d@(DataDcl n params cons) =
  do mapM (typeCons n params) cons
     return d
     
processDecl (PatBindDcl p e) = 
  do p' <- typePattern p
     e' <- typeExp e
     return $ PatBindDcl p' e'
     
processDecl (FunBindDcl _ _ _) = -- TODO Improve error handling
  error "Unexpected function declartion at this point"
processDecl d = return d

typeCons :: Name -> [Name] -> Constructor -> State MetaSt Constructor
typeCons n ps c@(IdConDcl i ts) =
  do MetaSt next env <- get
     put $ MetaSt next ((i, (toFun (ts ++ [ConType n ps']))):env)
     return c
     where
       toFun (t:[]) = t
       toFun (t:ts) = FunType t (toFun ts)
       ps' = map (\param -> (VarType param)) ps

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
typePattern (IdConPat n ids ts UnknownType) =
  do t <- getMeta 
     ts' <- getNMetas (length ids)
     addMetas ids ts'
     return $ IdConPat n ids ts' t

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
typeExp (IdVarExp i t) =
  do t' <- lookupId i
     return $ IdVarExp i (choose t t')

typeExp (IdConExp i t) =
  do t' <- lookupId i
     return $ IdConExp i (choose t t')

typeExp (FExp e1 e2 t) = 
  do e1' <- typeExp e1
     e2' <- typeExp e2
     t' <- getMeta
     return $ FExp e1' e2' (choose t t')
typeExp (LambdaExp ps e t) =
  do MetaSt _ env <- get
     ps' <- mapM typePattern ps
     e' <- typeExp e
     t' <- getMeta
     MetaSt num _ <- get
     put $ MetaSt num env
     return $ LambdaExp ps' e' (choose t t')
     
typeExp (LetExp decls e t) =
  do MetaSt _ env <- get 
     decls' <- mapM processDecl decls
     e' <- typeExp e
     t' <- getMeta
     MetaSt num _ <- get
     put $ MetaSt num env
     return $ LetExp decls' e' (choose t t')
     
typeExp (IfExp e1 e2 e3 t) =
  do e1' <- typeExp e1
     e2' <- typeExp e2
     e3' <- typeExp e3
     return $ IfExp e1' e2' e3' (choose t (getType e2'))

typeExp (CaseExp es alts t) = 
  do es' <- mapM typeExp es
     alts' <- mapM typeAlt alts
     t' <- getMeta
     return $ CaseExp es' alts' (choose t t')

typeExp (ParensExp e t) =
  do e' <- typeExp e
     return $ ParensExp e' (choose t (getType e'))
     
typeExp (TupleExp es t) =
  do es' <- mapM typeExp es
     return $ TupleExp es' (choose t (TupleType (map getType es')))
typeExp (ListExp es t) = 
  do es' <- mapM typeExp es
     t' <- getMeta
     return $ ListExp es' (choose t t')
typeExp e = return e

typeAlt (Alternative ps e) =
  do MetaSt _ env <- get
     ps' <- mapM typePattern ps
     e' <- typeExp e
     MetaSt num _ <- get
     put $ MetaSt num env
     return $ Alternative ps' e'

choose :: Type -> Type -> Type
choose UnknownType t = t
choose t _ = t


