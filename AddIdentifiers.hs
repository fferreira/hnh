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
module AddIdentifiers
  (
    addIdentifiers,
  )
  where

import Syntax
import TransformMonad(TransformM, transformOk)

-- TODO change the state monad, and pass an explicit parameter
-- it will be simpler
import Control.Monad.State(evalState, State, put, get)

addIdentifiers :: Program -> TransformM Program
addIdentifiers (Program decls) = transformOk "addIdentifiers" (Program decls')
  where
    decls' = evalState (processDecls decls) initialSt
    
data IdentSt = IdentSt {    
  nextVar :: Int
  , currEnv :: [(Name, Identifier)]
  }

initialSt = IdentSt 0 []

getId :: Name -> State IdentSt Identifier
getId n = 
  do IdentSt _ env <- get
     case lookup n env of Nothing -> error ("AddIdentifiers: Variable " 
                                            ++ n ++ " not found")
                          Just i -> return i

newId :: Name -> State IdentSt Identifier
newId n =
  do IdentSt next env <- get
     put (IdentSt (next + 1) ((n, (Id n next)):env))
     return (Id n next)
    
processDecls decls = mapM processDecl decls

processDecl (PatBindDcl p e) =
  do p' <- adaptPattern p
     e' <- adaptExp e
     return (PatBindDcl p' e')
-- processDecl (DataDcl t cons) =      
--   do cons' <- mapM adaptCons cons
--      return $ DataDcl t cons'
     
processDecl d = return d

-- adaptCons :: Constructor -> State IdentSt Constructor
-- adaptCons (ConDcl conName ts) = 
--   do i <- newId conName
--      return $ IdConDcl i ts

adaptPattern :: Pattern -> State IdentSt Pattern
adaptPattern (VarPat n t) =
  do i <- newId n
     return $ IdVarPat i t 
    
adaptPattern (ConPat n ns t) =
  do ids <- mapM newId ns
     return (IdConPat n ids [] t)
     
adaptPattern (TuplePat ns t) =
  do ids <- mapM newId ns
     return (IdTuplePat ids t)
     
adaptPattern (WildcardPat t) = return (WildcardPat t)     

adaptExp :: Exp -> State IdentSt Exp
adaptExp (VarExp n t) = 
  do i <- getId n
     return (IdVarExp i t)
       
adaptExp (ConExp n params t) = 
  do params' <- mapM getId params
     return (IdConExp n params' t)
     
adaptExp (Prim n params t) =     
  do params' <- mapM getId params
     return (IdPrim n params' t)
       
adaptExp (FExp e1 e2 t) =       
  do e1' <- adaptExp e1
     e2' <- adaptExp e2
     return $ FExp e1' e2' t
     
adaptExp (IfExp e1 e2 e3 t) =
  do e1' <- adaptExp e1
     e2' <- adaptExp e2
     e3' <- adaptExp e3
     return $ (IfExp e1' e2' e3' t)

adaptExp (ParensExp e t) = do e' <- adaptExp e ; return $ ParensExp e' t
adaptExp (TupleExp es t) = do es' <- mapM adaptExp es ; return $ TupleExp es' t
adaptExp (ListExp es t) = do es' <- mapM adaptExp es ; return $ ListExp es' t
adaptExp (LambdaExp pats e t) =
  do IdentSt _ env <- get
     pats' <- mapM adaptPattern pats
     e' <- adaptExp e
     IdentSt next _ <- get -- restore old env
     put $ IdentSt next env
     return $ LambdaExp pats' e' t

adaptExp (CaseExp es alts t) = 
  do es' <- mapM adaptExp es
     alts' <- mapM adaptAlt alts
     return $ CaseExp es' alts' t
adaptExp (LetExp decls e t) = 
  do IdentSt _ env <- get
     decls' <- processDecls decls
     e' <- adaptExp e
     IdentSt next _ <- get
     put $ IdentSt next env
     return $ LetExp decls' e' t
adaptExp e = return e

adaptAlt (Alternative pats e) =
  do IdentSt _ env <- get
     pats' <- mapM adaptPattern pats
     e' <- adaptExp e
     IdentSt next _ <- get
     put $ IdentSt next env
     return $ Alternative pats' e'