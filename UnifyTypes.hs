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
module UnifyTypes
  (
    unifyTypes
  )
  where
  
import Syntax
import GenerateConstraints(Constraint)
import ErrorMonad (ErrorM)

unifyTypes :: [Constraint] -> ErrorM [Subst]
unifyTypes cs = genSubst cs []

data UnificationRes = SameType
                    | NewSubst Subst

type Subst = (Type, Type) -- substitution put first type in place of the second

genSubst :: Monad m => [Constraint] -> [Subst] -> m[Subst]
genSubst ((t1, t2, d):cs) sub = 
  do res <- unify d t1 t2 
     case res of SameType -> genSubst cs sub -- ignore zero info substitutions
                 NewSubst s -> contWithSubst s cs sub
genSubst [] sub = return sub

contWithSubst :: Monad m => Subst -> [Constraint] -> [Subst] -> m[Subst]
contWithSubst (t1, t2) cs sub =  genSubst cs' ((t1,t2):sub')
  where
    sub' = repSubst sub
    cs' = repConst cs
    repSubst ((ta, tb):sub) = (rep ta, rep tb) : repSubst sub
    repSubst [] = []
    
    repConst ((ta, tb, d):cs) = (rep ta, rep tb, d): repConst cs
    repConst [] = []
    
    rep t = if (t == t2) then t1 else t

unify :: Monad m => Declaration -> Type -> Type -> m UnificationRes
unify d t1@(MetaType _) t2@(MetaType _) =
  if t1 == t2 then return SameType
  else return $ NewSubst (t1, t2)
       
unify d t1@(MetaType _) t2 = return $ NewSubst (t2, t1)
unify d t1 t2@(MetaType _) = return $ NewSubst (t1, t2)
       
unify d t1 t2 = fail ("Unable to unify "       
                      ++ show t1 ++ " and "
                      ++ show t2 ++ " in "
                      ++ show d) -- TOOD use pretty

test = [(MetaType 1, MetaType 2, nd)
       ,(ConType "Float" [], MetaType 12, nd)
       ,(MetaType 3, MetaType 4, nd)
       ,(MetaType 4, MetaType 12, nd)
       ,(MetaType 1, ConType "Int" [], nd)]
       
       
nd = TypeSigDcl [] UnknownType       