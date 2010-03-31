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
module ProgToLet
       (
         progToLet
       )
       where
  
import Syntax
import TransformMonad(TransformM, transformOk, transformError)
import ErrorMonad(ErrorM(..))
import TypeUtils(getType)

import Data.List(find, partition)


progToLet :: Program -> TransformM Program
progToLet (Program decls) = 
  case findMain decls of 
    Error msg -> transformError msg
    Success main -> transformOk "progToLet" (Program 
                                             (dataDecls
                                              ++ [mainToLet main varDecls]))
      where
        (varDecls, dataDecls) = partition isVarDecl decls

findMain :: Monad m => [Declaration] -> m Declaration
findMain decls = 
  case find isMain decls of Just d -> return d
                            Nothing -> fail "main symbol not found"
    where
      isMain (PatBindDcl p e) = case p of (IdVarPat (Id "main"_) _) -> True
                                          _ -> False
      isMain d = False

isVarDecl (PatBindDcl _ _) = True
isVarDecl _ = False

mainToLet :: Declaration -> [Declaration] -> Declaration
mainToLet d@(PatBindDcl p e) decls = 
  if length decls > 0 then
    (PatBindDcl p (LetExp decls e (getType e)))
  else
    d
    
