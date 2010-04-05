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
module Closure
       (
         closureConversion
       )
       where

import CpsRep
import TransformMonad (TransformM, transformOk)

import Data.List(nub)

-- closureConversion :: KExp -> TransformM KExp
closureConversion k = transformOk "closureConversion" ((nub .freeVars) k)


convert :: KExp -> KExp
convert (LitK val i k t) = (LitK val i (convert k) t)
convert (VarK i v k t) = (VarK i v (convert k) t)
convert (TupDK i n v k t) = (TupDK i n v (convert k) t)
convert k = k

freeVars :: KExp -> [Identifier]            
freeVars (LitK val v k t) = (freeVars k) `subs` [v]
freeVars (VarK i v k t) = i:((freeVars k) `subs` [i, v])
freeVars (IfK i k1 k2) = i:(freeVars k1 ++ freeVars k2)
freeVars (TupDK i n v k t) = i:(freeVars k `subs` [i, v]) 
freeVars (ConDK i n v k t) = i:(freeVars k `subs` [i, v])
freeVars (AppK i params) = i:params
freeVars (FunK params body v k) = ((freeVars body `subs` params) ++ freeVars k) `subs` [v]
freeVars (TupleK ids v k) = ids ++ (freeVars k `subs` [v])
freeVars (ListK ids v k) = ids ++ (freeVars k `subs` [v])
freeVars (SwitchK ids alts) = ids ++ (nub (concatMap freeAltVars alts)  `subs` ids)
freeVars (HaltK) = []

freeAltVars :: AltK -> [Identifier]
freeAltVars (AltK cond k) = freeVars k




{-
  subs returns a list of all the elements of a that are not in b
-}
subs :: Eq a => [a] -> [a] -> [a]
subs a b = filter (\e -> e `notElem` b) a
