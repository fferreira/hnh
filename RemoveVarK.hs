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

module RemoveVarK
       (
         removeVarK
       )
       where

import CPSRep
import TransformMonad (TransformM, transformOk)

import Control.Monad.State (State, get, put, evalState)

import Tools

removeVarK :: KExp -> TransformM KExp
removeVarK k = transformOk "removeVarK" (evalState (procK k) [])
  
type Subst = (Identifier, Identifier)

add :: Identifier -> Identifier -> State [Subst] ()
add a b = do st <- get
             put ((a,b):st)
             
rep :: Identifier -> State [Subst] Identifier             
rep v = do st <- get
           case lookup v st of Nothing -> return v
                               Just i -> return i

procK :: KExp -> State [Subst] KExp
procK (VarK i v k t) = do add v i
                          procK k
                                
procK (IfK i k1 k2) =
  do i' <- rep i
     k1' <- procK k1
     k2' <- procK k2
     return (IfK i' k1' k2')
     
procK (LitK val i k t) = 
  do i' <- rep i
     k' <- procK k
     return (LitK val i' k' t)
     
procK (TupDK i n v k t) =     
  do i' <- rep i
     k' <- procK k
     return (TupDK i' n v k' t)
     
procK (ConDK i n v k t) =      
  do i' <- rep i
     k' <- procK k
     return (ConDK i' n v k' t)
     
procK (PrimK i k t) =
  do i' <- rep i
     k' <- procK k
     return (PrimK i' k' t)
     
procK (AppK i ids) =
  do i' <- rep i
     ids' <- mapM rep ids
     return (AppK i' ids')
     
procK (FunK ids body v k) = 
  do ids' <- mapM rep ids
     body' <- procK body
     k' <- procK k
     return (FunK ids' body' v k')
     
procK (TupleK ids v k) =      
  do ids' <- mapM rep ids
     k' <- procK k
     return (TupleK ids' v k')
     
procK (ListK ids v k) =
  do ids' <- mapM rep ids
     k' <- procK k
     return (ListK ids' v k')
     
procK (SwitchK ids alts) =
  do ids' <- mapM rep ids
     alts' <- mapM procAltK alts
     return (SwitchK ids' alts')

procK HaltK = return HaltK

procAltK (AltK conds k) =
  do k' <- procK k
     return (AltK conds k')