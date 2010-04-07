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

import CPSRep
import TransformMonad (TransformM, transformOk)
import ProgramUtils(getNextIdNumKExp)

import Control.Monad.State(State, get, put, evalState)
import Data.List(nub)

closureConversion :: KExp -> TransformM KExp
closureConversion k = transformOk "closureConversion" (evalState 
                                                       (convert k) 
                                                       (getNextIdNumKExp k)
                                                      )

type CloSt = Int

newVar :: State CloSt Identifier
newVar = do next <- get
            put $ next + 1
            return (Id "clo" next)

convert :: KExp -> State CloSt KExp
convert (LitK val i k t) = 
  do k' <- convert k
     return (LitK val i k' t)
convert (VarK i v k t) = 
  do k' <- convert k
     return (VarK i v k' t)
convert (TupDK i n v k t) = 
  do k' <- convert k
     return (TupDK i n v k' t)
convert (ConDK i n v k t) =       
  do k' <- convert k
     return (ConDK i n v k' t)
convert (PrimK i k t) =     
  do k' <- convert k
     return (PrimK i k' t)
convert (AppK f params) = 
  do f0 <- newVar
     return (TupDK f 0 f0 (AppK f0 (f:params)) UnknownType)

convert fun@(FunK f params body k) =
  do body' <- convert body
     k' <- convert k
     f' <- newVar
     cp <- newVar -- closure parameter
     body'' <- convertBody cp (freeFunKVars fun) body'
     return (FunK f' (cp:params) body'' 
             (TupleK (f':freeFunKVars fun) f k'))

convert (TupleK ids v k) =
  do k' <- convert k
     return (TupleK ids v k')
convert (ListK ids v k) =
  do k' <- convert k
     return (ListK ids v k')
convert (SwitchK ids alts) =
  do alts' <- mapM convertAlt alts
     return (SwitchK ids alts')
convert HaltK = return HaltK
convert k = return k 

convertAlt (AltK conds k) =
  do k' <- convert k
     return (AltK conds k')
     
convertBody :: Identifier -> [Identifier] -> KExp -> State CloSt KExp
convertBody closure fvs body = convertBody' closure fvs 1 body
  where
    convertBody' :: Identifier -> [Identifier] -> 
                    Int -> KExp -> State CloSt KExp
    convertBody' closure [] n body = return body
    convertBody' closure [fv] n body =
      return $ TupDK closure n fv body UnknownType
    convertBody' closure (fv:fvs) n body =
      do k <- convertBody' closure fvs (n+1) body
         return $ TupDK closure n fv k UnknownType -- TODO improve typing

freeFunKVars (FunK v params body k) = freeVars body `subs` (v:params)
freeFunKVars k = error ("Unexpected, not a function: " ++ show k)

-- freeVars returns the free variables of a KExp
freeVars :: KExp -> [Identifier]            
freeVars e = (nub . freeVars') e
  where
    freeVars' (LitK val v k t) = (freeVars' k) `subs` [v]
    freeVars' (VarK i v k t) = i:((freeVars' k) `subs` [i, v])
    freeVars' (IfK i k1 k2) = i:(freeVars' k1 ++ freeVars' k2)
    freeVars' (TupDK i n v k t) = i:(freeVars' k `subs` [i, v]) 
    freeVars' (ConDK i n v k t) = i:(freeVars' k `subs` [i, v])
    freeVars' (PrimK i k t) = []
    freeVars' (AppK i params) = i:params
    freeVars' (FunK v params body k) = 
      ((freeVars' body `subs` params) ++ freeVars' k) `subs` [v]
    freeVars' (TupleK ids v k) = ids ++ (freeVars' k `subs` [v])
    freeVars' (ListK ids v k) = ids ++ (freeVars' k `subs` [v])
    freeVars' (SwitchK ids alts) = 
      ids ++ ((concatMap freeAltVars alts)  `subs` ids)
    freeVars' (HaltK) = []

    freeAltVars :: AltK -> [Identifier]
    freeAltVars (AltK cond k) = freeVars' k

{-
  subs returns a list of all the elements of a that are not in b
-}
subs :: Eq a => [a] -> [a] -> [a]
subs a b = filter (\e -> e `notElem` b) a
