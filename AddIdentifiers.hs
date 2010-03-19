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
module AddIdentifiers
  (
    addIdentifiers,
    idEnv0
  )
  where

import Syntax
import TransformMonad(TransformM, transformOk)
import BuiltIn(env0)

import Control.Monad.State(evalState, State, put, get)

addIdentifiers :: Program -> TransformM Program
addIdentifiers (Program decls) = transformOk "addIdentifiers" (Program decls')
  where
    decls' = evalState (processDecls decls) initialSt
    
data IdentSt = IdentSt {    
  nextVar :: Int
  , currEnv :: [(Name, Identifier)]
  }

-- initialSt taking into account the env0 of builtIns --TODO check out this
initialSt = IdentSt (length env0) (map 
                                   (\(n,num) -> (n, Id n num)) 
                                   (zip 
                                    (fst (unzip env0)) 
                                    [0..(length env0 - 1)]))
            
idEnv0 :: [(Identifier, Type)]
idEnv0 = map (\(n, t) -> (findId n, t)) env0
  where
    findId n = case lookup n (currEnv initialSt) of
      Just i -> i
      Nothing -> error "Unexpected"
                  
getEnv :: State IdentSt [(Name, Identifier)]                  
getEnv = get >>= (return. currEnv)
    
getNext :: State IdentSt Int
getNext = get >>= (return . nextVar)
    
processDecls decls = mapM processDecl decls

processDecl (PatBindDcl p e) =
  do p' <- adaptPattern p
     e' <- adaptExp e
     return (PatBindDcl p' e')
processDecl (DataDcl t cons) =      
  do cons' <- mapM adaptCons cons
     return $ DataDcl t cons'
     
processDecl d = return d

adaptCons :: Constructor -> State IdentSt Constructor
adaptCons (ConDcl conName ts) = 
  do IdentSt next env <- get
     i <- return $ Id conName next
     put $ IdentSt (next + 1) ((conName, i):env)
     return $ IdConDcl i ts

adaptPattern :: Pattern -> State IdentSt Pattern
adaptPattern (VarPat n t) =
  do IdentSt next env <- get
     put $ IdentSt (next + 1) ((n, Id n next):env)
     return $ IdVarPat (Id n next) t
    
adaptPattern (ConPat n ns t) =
  do IdentSt next env <- get
     ids <- mapM (\(n,num) -> return $ Id n num) (zip ns [next..(next + length ns)])
     put $ IdentSt (next + (length ns)) ((zip ns ids) ++ env)
     return $ IdConPat n ids [] t
     
adaptPattern (TuplePat ns t) =
  do IdentSt next env <- get
     ids <- mapM (\(n,num) -> return $ Id n num) (zip ns [next..(next + length ns)])
     put $ IdentSt (next + (length ns)) ((zip ns ids) ++ env)
     return $ IdTuplePat ids t

adaptExp :: Exp -> State IdentSt Exp
adaptExp (VarExp n t) = 
  do env <- getEnv
     id <- return $ lookup n env
     case id of
       (Just id) -> return $ IdVarExp id t
       Nothing -> error ("Variable " ++ n ++ " not found")
       
adaptExp (ConExp n t) = 
  do env <- getEnv
     id <- return $ lookup n env
     case id of
       (Just id) -> return $ IdConExp id t
       Nothing -> error ("Variable " ++ n ++ " not found")
       
       
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
     IdentSt next _ <- get
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