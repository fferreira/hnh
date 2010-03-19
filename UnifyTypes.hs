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
  , Subst
  )
  where
  
import Syntax
import GenerateConstraints(Constraint)
import ErrorMonad (ErrorM)

import Text.PrettyPrint.Leijen -- requires wl-pprint installed (available in cabal)

unifyTypes :: [Constraint] -> ErrorM [Subst]
unifyTypes cs = genSubst cs []

data UnificationRes = SameType
                    | New Subst
                    | Push [Constraint]

type Subst = (Type, Type) -- substitution put first type in place of the second

genSubst :: Monad m => [Constraint] -> [Subst] -> m[Subst]
genSubst ((t1, t2, d):cs) sub = 
  do res <- unify d t1 t2 
     case res of SameType -> genSubst cs sub -- ignore zero info substitutions
                 New s -> contWithSubst s cs sub
                 Push c -> genSubst (c++cs) sub
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
    
    rep = typeRep (t1, t2) -- TODO find types within types!

typeRep :: (Type, Type) -> Type -> Type      
-- looks for t in t2 if so then it replaces by t1 otherwise it return t
typeRep c@(t1, t2) (FunType ta tb) = (FunType (typeRep c ta) (typeRep c tb))
typeRep (t1, t2) t = if t == t2 then t1 else t -- COnsider composit t's
      
unify :: Monad m => Declaration -> Type -> Type -> m UnificationRes
unify d t1@(MetaType _) t2@(MetaType _) =
  if t1 == t2 then return SameType
  else return $ New (t1, t2)
       
unify d t1@(MetaType _) t2 = return $ New (t2, t1)
unify d t1 t2@(MetaType _) = return $ New (t1, t2)

unify d (FunType t11 t12) (FunType t21 t22) = 
  return $ Push [(t11, t21, d), (t12, t22, d)]
       
unify d t1 t2 = 
  if t1 == t2 then return $ SameType
  else fail $ show (pretty "Unable to unify"
                    <+> pretty t1 <+> pretty "and"
                    <+> pretty t2 <> line
                    <> pretty "in" <+> pretty d)

