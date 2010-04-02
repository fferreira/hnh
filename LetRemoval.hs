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
     
processExp e = return e