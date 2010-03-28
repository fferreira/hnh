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
module InferDeclType
       (
         inferDeclType'
       )
       where

import Syntax
import AddIdentifiers(idEnv0)
import AddMetaTypes(declarationMeta)
import GenerateConstraints(declarationConstraints)
import UnifyTypes(unifyTypes)
import Substitutions(replaceInDecl)
import GeneralizeTypes(generalizeTypes)
import ErrorMonad(ErrorM(..))

import Tools

inferDeclType' :: Declaration -> Declaration
inferDeclType' d = generalizeTypes d'
  where
    dts = []
    env = idEnv0
    metaD = fst (declarationMeta dts env d)
    constraints = declarationConstraints metaD
    subs = case unifyTypes constraints of Success subs -> subs
                                          --TODO improve error handling
                                          Error msg -> error msg 
    d' = replaceInDecl (id $ subs) d
