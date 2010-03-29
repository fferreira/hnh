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
module GeneralizeTypes
       (
         generalizeTypes
       )
       where

import Syntax
import TypeUtils(getType, addType, getPatType, addPatType)

import Control.Monad.State(evalState, State, get, put)

generalizeTypes :: Declaration -> Declaration
generalizeTypes d =
  evalState (transDecl d) (GenSt 0 [])

data GenSt = GenSt Int [(Type, Type)] 

transType :: Type -> State GenSt Type
transType t@(MetaType _) =
  do GenSt next ts <- get
     case lookup t ts of Just t' -> return t'
                         Nothing -> do put $ 
                                         GenSt 
                                         (next + 1) 
                                         ((t, getVarType next):ts)
                                       return $ getVarType next
transType (FunType t1 t2) =
  do t1' <-transType t1 
     t2' <- transType t2
     return $ FunType t1' t2'
transType (TupleType ts) =
  do ts' <- mapM transType ts
     return $ TupleType ts'
transType t@(VarType _) = return t
transType t@(DataType n ts) =
  do ts' <- mapM transType ts
     return (DataType n ts')
transType t = return t
                          
getVarType :: Int -> Type
getVarType i =
  if i >= length letters 
  then VarType ("var#" ++ show i)
  else VarType ((letters !! i):"#")
    where
    letters = "abcdefghijklmnopqrstuvwxyz"
    
    
transPat p = do t <- transType (getPatType p)    
                return (addPatType p t)
                
transExp (FExp e1 e2 t) =
  do e1' <- transExp e1
     e2' <- transExp e2
     t' <- transType t
     return (FExp e1' e2' t')
     
transExp (LambdaExp pats e t) = 
  do pats' <- mapM transPat pats
     e' <- transExp e
     t' <- transType t
     return (LambdaExp pats' e' t')
     
transExp (LetExp decls e t) = 
  do decls' <- mapM transDecl decls
     e' <- transExp e
     t' <- transType t
     return (LetExp decls' e' t')
     
transExp (IfExp e1 e2 e3 t) =
  do [e1', e2', e3'] <- mapM transExp [e1, e2, e3]
     t' <- transType t
     return (IfExp e1' e2' e3' t')
     
transExp (CaseExp es alts t) = 
  do es' <- mapM transExp es
     alts' <- mapM transAlt alts
     t' <- transType t
     return (CaseExp es' alts' t')
     
transExp (ParensExp e t) =
  do e' <- transExp e
     t' <- transType t
     return (ParensExp e' t')

transExp (TupleExp es t) =
  do es' <- mapM transExp es
     t' <- transType t
     return (TupleExp es' t')
     
transExp (ListExp es t) =
  do es' <- mapM transExp es
     t' <- transType t
     return (ListExp es' t')
     
transExp e = do t <- transType (getType e) -- TODO do it recursively  
                return (addType e t)
    
transAlt (Alternative pats e) = do pats' <- mapM transPat pats
                                   e' <- transExp e
                                   return (Alternative pats' e')

transDecl (PatBindDcl p e) = 
  do p' <- transPat p
     e' <- transExp e
     return (PatBindDcl p' e')
transDecl (FunBindDcl _ _ _) = error "Unexpected function declaration"     
transDecl d = return d