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
module GenerateConstraints
       (
         generateConstraints
       , Constraint
       )
       where

import Syntax
import TypeUtils(getType, getPatType)

import Control.Monad.State(execState, State, get, put)

type Constraint = (Type, Type)

addConstraint :: Type -> Type -> State [Constraint] ()
addConstraint t1 t2 = 
  do st <- get
     put ((t1, t2):st)

generateConstraints :: Program -> [Constraint]
generateConstraints (Program decls) = 
  execState (processDecls decls) []

processDecls :: [Declaration] -> State [Constraint] ()
processDecls decls = do mapM processDecl decls; return ()

processDecl (PatBindDcl pat e) = 
  do addConstraint (getPatType pat) (getType e)
processDecl decl = return ()
