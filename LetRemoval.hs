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
module LetRemoval
       (
         letRemoval
       )
       where

import Syntax
import TransformMonad(TransformM, transformOk, transformError)
import TypeUtils(isVarDecl)

import Data.List(partition)
import Control.Monad.State(runState, State, get, put)

letRemoval :: Program -> TransformM Program
letRemoval (Program decls) = transformOk 
                             "LetRemoval" 
                             (Program (dataD ++ (processDecls varD)))
                               where
                                 (varD, dataD) = partition isVarDecl decls
                
processDecls :: [Declaration] -> [Declaration]
processDecls decls = extraDecls ++ decls'
  where
    (decls', extraDecls) = runState (mapM processDecl decls) []

processDecl :: Declaration -> State [Declaration] Declaration
processDecl (PatBindDcl p e) = 
  do e' <- processExp e
     return (PatBindDcl p e')
processDecl d = return d

processExp :: Exp -> State [Declaration] Exp
processExp (LetExp decls e t) = 
  do st <- get 
     decls' <- mapM processDecl decls
     put (decls' ++ st)
     processExp e
     
processExp (FExp e1 e2 t) =
  do e1' <- processExp e1
     e2' <- processExp e2
     return (FExp e1' e2' t)
     
processExp (LambdaExp pats e t) =     
  do e' <- processExp e
     return (LambdaExp pats e' t)
     
processExp (IfExp ec e1 e2 t) =     
  do ec' <- processExp ec
     e1' <- processExp e1
     e2' <- processExp e2
     return (IfExp ec' e1' e2' t)
     
processExp (CaseExp es alts t) =     
  do es' <- mapM processExp es
     alts' <- mapM processAlt alts
     return (CaseExp es' alts' t)
     
processExp (ParensExp e t) =
  do e' <- processExp e
     return (ParensExp e' t)
     
processExp (TupleExp es t) =     
  do es' <- mapM processExp es
     return (TupleExp es' t)
     
processExp (ListExp es t) =     
  do es' <- mapM processExp es
     return (ListExp es' t)
processExp e = return e

processAlt (Alternative pats e) =
  do e' <- processExp e
     return (Alternative pats e')