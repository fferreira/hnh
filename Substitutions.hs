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
         replaceInDecl
       )
       where

import Syntax
import GenerateConstraints(generateConstraints)
import UnifyTypes(unifyTypes, Subst)
import TypeUtils(addType, getType, addPatType, getPatType)
import TransformUtils(transformTree, Transformer(..), defTrans)
import ErrorMonad(ErrorM(..))
import Control.Monad.State(get, State, evalState)

import Tools
  
replaceInDecl :: [Subst] -> Declaration -> Declaration
replaceInDecl subs (PatBindDcl p e) = 
  PatBindDcl (replaceInPat subs p) (replaceInExp subs e)
replaceInDecl _ (FunBindDcl _ _ _) = error "Not expected function declaration"  
replaceInDecl _ d = d

replaceInExp :: [Subst] -> Exp -> Exp
replaceInExp subs (FExp e1 e2 t) = (FExp 
                                    (replaceInExp subs e1)
                                    (replaceInExp subs e2)
                                    (repType subs t))
                                 
replaceInExp subs (LambdaExp pats e t) = (LambdaExp 
                                          (map (replaceInPat subs) pats)
                                          (replaceInExp subs e)
                                          (repType subs t))

replaceInExp subs (LetExp decls e t) = (LetExp
                                        (map (replaceInDecl subs) decls)
                                        (replaceInExp subs e)
                                        (repType subs t))

replaceInExp subs (IfExp e1 e2 e3 t) = (IfExp
                                        (replaceInExp subs e1)
                                        (replaceInExp subs e3)
                                        (replaceInExp subs e3)                                   
                                        (repType subs t))

replaceInExp subs (CaseExp es alts t) = (CaseExp
                                         (map (replaceInExp subs) es)
                                         (map (replaceInAlt subs) alts)
                                         (repType subs t))
                                        
replaceInExp subs (ParensExp e t) = (ParensExp
                                     (replaceInExp subs e)
                                     (repType subs t))
                                    
replaceInExp subs (TupleExp es t) = (TupleExp
                                     (map (replaceInExp subs) es)
                                     (repType subs t))

replaceInExp subs (ListExp es t) = (ListExp
                                    (map (replaceInExp subs) es)
                                    (repType subs t))
                                  
replaceInExp subs e  = addType e (repType subs (getType e))

replaceInPat :: [Subst] -> Pattern -> Pattern
replaceInPat subs (IdConPat n ids ts t) = (IdConPat n ids
                                           (map (repType subs) ts)
                                           (repType subs t))
                                           
replaceInPat subs p = addPatType p (repType subs (getPatType p))

replaceInAlt :: [Subst] -> Alternative -> Alternative
replaceInAlt subs (Alternative pats e) = (Alternative
                                          (map (replaceInPat subs) pats)
                                          (replaceInExp subs e))
                                  
repType :: [Subst] -> Type -> Type
repType subs t = evalState (rep' t) subs

rep' :: Type -> State [Subst] Type
rep' t@(FunType t1 t2) =
  repOrAct t (do t1' <- rep' t1
                 t2' <- rep' t2
                 return (FunType t1' t2'))

rep' t@(TupleType ts) =
  repOrAct t (do ts' <- mapM rep' ts
                 return (TupleType ts'))
  
rep' t@(DataType n ts) =
  repOrAct t (do ts' <- mapM rep' ts
                 return (DataType n ts'))
  
rep' t = repOrAct t (return t)
       

repOrAct :: Type -> State [Subst] Type -> State [Subst] Type              
repOrAct t act = do subs <- get
                    case lookup t (inv subs) of
                      Just t' -> return t'
                      Nothing -> act
                 where
                   invPair (a,b) = (b,a)       
                   inv = ((\(a,b) -> zip a b) . invPair . unzip)