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
    addIdentifiers
  )
  where

import Syntax
import TransformMonad(TransformM, transformOk)
import Control.Monad.State(evalState, State, put, get)

addIdentifiers :: Program -> TransformM Program
addIdentifiers (Program decls) = transformOk "addIdentifiers" (Program decls')
  where
    decls' = evalState (processDecls decls) initialState
    
data IdentState = IdentState {    
  nextVar :: Int
  , env :: [(Name, Identifier)]
  }
                  
initialState = IdentState 0 []
    
processDecls decls = mapM processDecl decls

processDecl (PatBindDcl pats e) = undefined
processDecl d = return d
  

