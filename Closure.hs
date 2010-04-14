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

import Tools

closureConversion :: KExp -> TransformM KExp
closureConversion k = transformOk "closureConversion" (evalState 
                                                       (closureConvert k) 
                                                       (getNextIdNumKExp k)
                                                      )

type CloSt = Int

newVarNum :: State CloSt Int
newVarNum = do next <- get
               put $ next + 1
               return next
               
newVar :: State CloSt Identifier               
newVar = do num <- newVarNum
            return (Id "clo" num)
            
newVarFrom :: Identifier -> State CloSt Identifier        
newVarFrom (Id name _) =
  do num <- newVarNum
     return (Id ("c-" ++ name ++ ".") num)

data Conv = CAppK | CFunK deriving (Show, Eq)

closureConvert :: KExp -> State CloSt KExp
closureConvert k = do k' <- convert CAppK k
                      convert CFunK k'
  where
    convert :: Conv -> KExp -> State CloSt KExp
    convert c (LitK val i k) = 
      do k' <- convert c k
         return (LitK val i k')
         
    convert c (VarK i v k) = 
      do k' <- convert c k
         return (VarK i v k')
         
    convert c (TupDK i n v k) = 
      do k' <- convert c k
         return (TupDK i n v k')
         
    convert c (ConDK i n v k) =       
      do k' <- convert c k
         return (ConDK i n v k')
         
    convert c (PrimK i k) =     
      do k' <- convert c k
         return (PrimK i k')
         
    convert c k@(AppK f params) = 
      if c == CAppK then convertAppK k else return k

    convert c fun@(FunK f params body k) = 
      if c == CFunK then convertFunK c fun 
      else
        do body' <- convert c body
           k' <- convert c k
           return (FunK f params body' k')

    convert c (TupleK ids v k) =
      do k' <- convert c k
         return (TupleK ids v k')
         
    convert c (ListK ids v k) =
      do k' <- convert c k
         return (ListK ids v k')
         
    convert c (SwitchK ids alts) =
      do alts' <- mapM (convertAlt c) alts
         return (SwitchK ids alts')
         
    convert c (HaltK i) = return (HaltK i)
    
    convert c k = return k 

    convertAlt c (AltK conds k) =
      do k' <- convert c k
         return (AltK conds k')
         
    convertAppK :: KExp -> State CloSt KExp
    convertAppK (AppK f params) = 
      do f0 <- newVarFrom f
         return (TupDK f 0 f0 (AppK f0 (f:params)))
    
    convertFunK c fun@(FunK f params body k) =
      do body' <- convert c body
         k' <- convert c k
         f' <- newVarFrom f
         cp <- newVar -- closure parameter
     
         fvs <- return $ traceP' ("fv in:" ++  show f)(freeFunKVars fun) -- free variables
         newFvs <- mapM (\fv -> newVarFrom fv) fvs -- new name for free vars
     
         body'' <- convertBody cp newFvs (cpsRep (zip fvs newFvs)
                                          body')
     
         return (FunK f' (cp:params) body''
                 (TupleK (f':fvs) f k'))

     
    convertBody :: Identifier -> [Identifier] -> KExp -> State CloSt KExp
    convertBody closure fvs body = 
      convertBody' closure fvs 1 body
        where
          convertBody' :: Identifier -> [Identifier] 
                          -> Int -> KExp -> State CloSt KExp
          convertBody' closure [] n body = return body 
          convertBody' closure [fv] n body =
            return (TupDK closure n fv body)
          convertBody' closure (fv:fvs) n body = 
            do k <- convertBody' closure fvs (n+1) body
               error "pum"
               return (TupDK closure n fv k)

freeFunKVars (FunK v params body k) = freeVars body `subs` (v:params)
freeFunKVars k = error ("Unexpected, not a function: " ++ show k)

-- freeVars returns the free variables of a KExp
freeVars :: KExp -> [Identifier]            
freeVars e = (nub . freeVars') e
  where
    freeVars' (LitK val v k) = (freeVars' k) `subs` [v]
    freeVars' (VarK i v k) = i:((freeVars' k) `subs` [i, v])
    freeVars' (IfK i k1 k2) = i:(freeVars' k1 ++ freeVars' k2)
    freeVars' (TupDK i n v k) = i:(freeVars' k `subs` [i, v]) 
    freeVars' (ConDK i n v k) = i:(freeVars' k `subs` [i, v])
    freeVars' (PrimK i k) = freeVars' k
    freeVars' (AppK i params) = i:params
    freeVars' (FunK v params body k) = 
      ((freeVars' body `subs` params) ++ freeVars' k) `subs` [v]
    freeVars' (TupleK ids v k) = ids ++ (freeVars' k `subs` [v])
    freeVars' (ListK ids v k) = ids ++ (freeVars' k `subs` [v])
    freeVars' (SwitchK ids alts) = 
      ids ++ ((concatMap freeAltVars alts)  `subs` ids)
    freeVars' (HaltK i) = [i]

    freeAltVars :: AltK -> [Identifier]
    freeAltVars (AltK cond k) = freeVars' k

{-
  subs returns a list of all the elements of a that are not in b
-}
subs :: Eq a => [a] -> [a] -> [a]
subs a b = filter (\e -> e `notElem` b) a


cpsRep :: [(Identifier, Identifier)] -> KExp -> KExp
cpsRep dict e = cr e
  where
    cr (IfK i k1 k2) = IfK (rep i) (cr k1) (cr k2)
    cr (LitK val i k) = LitK val (rep i) (cr k)
    cr (VarK i i' k) = VarK (rep i) (rep i') (cr k)
    cr (TupDK i n i' k) = TupDK (rep i) n (rep i') (cr k)
    cr (ConDK i n i' k) = ConDK (rep i) n (rep i') (cr k)
    cr (PrimK i k) = PrimK (rep i) (cr k)
    cr (AppK i ids) = AppK (rep i) (map rep ids)
    cr (FunK i ids k1 k2) = FunK i (map rep ids) (cr k1) (cr k2)
    cr (TupleK ids i k) = TupleK (map rep ids) (rep i) (cr k)
    cr (ListK ids i k) = ListK (map rep ids) (rep i) (cr k)
    cr (SwitchK ids alts) = SwitchK (map rep ids) (map ar alts)
    cr (HaltK i) = HaltK (rep i)
    
    ar (AltK conds k) = AltK conds (cr k)
    
    rep i = case lookup i dict of Nothing -> i
                                  Just i' -> i'
    
                        
                        
                        