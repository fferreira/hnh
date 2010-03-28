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
module Substitutions
       (
         performSubstitutions
       , replaceInDecl
       )
       where

import Syntax
import GenerateConstraints(generateConstraints)
import UnifyTypes(unifyTypes, Subst)
import TypeUtils(addType, getType, addPatType, getPatType)
import TransformMonad (TransformM, transformOk, transformError)
import TransformUtils(transformTree, Transformer(..), defTrans)
import ErrorMonad(ErrorM(..))

import Tools
  
performSubstitutions :: Program -> TransformM Program
performSubstitutions prog = transformTree
                            "performSubstitutions"
                            defTrans { tExp = (return . (replaceInExp subs))
                                     , tPat = (return . (replaceInPat subs)) }
                            prog
                              where
                                constraints =  generateConstraints prog
                                subs = case unifyTypes constraints of
                                  Success ss -> ss
                                  Error msg -> error msg --TODO improve error handling
                                  

replaceInDecl :: [Subst] -> Declaration -> Declaration
replaceInDecl subs (PatBindDcl p e) = 
  PatBindDcl (replaceInPat subs p) (replaceInExp subs e)
replaceInDecl _ (FunBindDcl _ _ _) = error "Not expected function declaration"  
replaceInDecl _ d = d

replaceInExp :: [Subst] -> Exp -> Exp
replaceInExp subs e  = addType e (rep subs (getType e))

replaceInPat :: [Subst] -> Pattern -> Pattern
replaceInPat subs p = addPatType p (rep subs (getPatType p))
                                  
rep :: [Subst] -> Type -> Type
rep ((t1, t2):subs) t = if t == t2 then t1 else rep subs t
rep [] t = t
