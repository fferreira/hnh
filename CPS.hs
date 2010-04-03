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
module CPS
       (
         cpsTransform
       )
  where

import Syntax
import CpsRep
import TransformMonad (TransformM, transformOk)

import Control.Monad.State(evalState, State, get, put)
import Data.List(find)


import Tools

cpsTransform :: Program -> TransformM KExp
cpsTransform p@(Program decls) = transformOk "cps" (getMainK conts)
  where
    conts = concatMap declToK decls

declToK :: Declaration -> [(Identifier, (Identifier, KExp) -> KExp)]
declToK (PatBindDcl (IdVarPat i _{-t-}) e) = [(i, cpsConv e)]
declToK d = []

getMainK :: [(Identifier, (Identifier, KExp) -> KExp)] -> KExp
getMainK conts  = 
  case find (\((Id n _), _) -> n == "main") conts of
    Just (_, f) -> f (Id "end" 42, HaltK) --TODO what id should it be passed?
    Nothing -> error "Unable to find function main"
  
type CPSSt = Int

cpsConv :: Exp  -> (Identifier, KExp) -> KExp
cpsConv e (v, k) = evalState (cps e (v, k)) initialSt

initialSt = 1000 -- TODO put the correct number here

newVar :: State CPSSt Identifier
newVar = do next <- get
            put (next + 1)
            return (Id "cont" next)
            
newVars :: Int -> State CPSSt [Identifier]            
newVars 1 = do v <- newVar; return [v]
newVars n = do v <- newVar
               rest <- newVars (n-1) 
               return (v:rest)

cps :: Exp  -> (Identifier, KExp) -> State CPSSt KExp
cps (LitExp val t) (v, k) = return $ LitK val v k t -- TODO is this correct?
cps (FExp e1 e2 t) (v, k) = 
  do xk <- newVar
     f <- newVar -- the function
     x <- newVar -- its parameter
     
     ek <- link x e2 (v, FunK [v] k xk (AppK f [x,xk]))
     link f e1 (v, ek) 
     
cps (LambdaExp pats e t) (v, k) = 
  do xk <- newVar
     res <- newVar
     ke <- cps e (res, AppK xk [res])
     return (FunK ((map patToId pats) ++ [xk]) ke v k)
     
     
cps (LetExp decls e t) (v, k) =
  do exit <- newVar 
     ke <- cps e (exit, k)
     linkL ids exps (v, ke)
       where
         (ids, exps) = unzip $ map (\(PatBindDcl (IdVarPat i _) e) -> (i, e)) decls

cps (IfExp ec e1 e2 t) (v, k) = 
  do xc <- newVar -- result of the condition
     f <- newVar  -- variable that will hold the continuation function
     r <- newVar -- result of the if for the rest of the program  
     e1' <- (cps e1 (r, AppK f [r]))
     e2' <- (cps e2 (r, AppK f [r]))
     
     ec' <- cps ec (xc, IfK xc e1' e2')
     
     return $ FunK [v] k f ec'

cps (CaseExp es alts t) (v, k) = 
  do ids <- newVars(length es)
     linkL <- newVars (length es)
     conds <- mapM (cpsAlt (v,k))  alts
     return $ SwitchK ids conds
     
     
cps (ParensExp e t) (v, k) = cps e (v,k)
cps (TupleExp es t) (v, k) = 
  do ids <- newVars (length es)
     linkL ids es (v, (TupleK ids v k))
cps (ListExp es t) (v, k) = 
  do ids <- newVars (length es)
     linkL ids es (v, (ListK ids v k))
  
cps (IdVarExp i t) (v, k) = return $ VarK i v k t
  -- do f <- newVar
  --    return $ FunK [v] k f (AppK f [i]) --TODO what about VarK?
cps (IdConExp i t) (v, k) = return $ VarK i v k t -- TODO is this correct?

cps (VarExp _ _) (_, _)        = error "Unexpected VarExp"
cps (ConExp _ _) (_, _)        = error "Unexpected ConExp"
cps (InfixOpExp _ _) (_, _)    = error "Unexpected InfixOpExp"
cps (MinusFloatExp _ _) (_, _) = error "Unexpected MinusFloatExp"
cps (MinusExp _ _) (_, _)      = error "Unexpected MinusExp"

cpsL :: [Exp] -> [Identifier]
cpsL = undefined

cpsAlt :: (Identifier, KExp) -> Alternative -> State CPSSt AltK
cpsAlt (v,k) (Alternative pats e) = 
  do e' <- cps e (v, k)
     return $ AltK (map patToCond pats) e' --TODO add link for the linked vars

patToCond :: Pattern -> CondK
patToCond (IdVarPat _ _) = WildK
patToCond (IdTuplePat _ _) = WildK
patToCond (IdConPat n _ _ _) = ConK n


{-
 link takes an identifier, a variable and a continuation
 it executes the continuation where the variable is bound to the expresion
-}
link :: Identifier -> Exp -> (Identifier, KExp)  -> State CPSSt KExp
link i e (v, k)=
  do f <- newVar
     cps e (v, (FunK [i] k f  (AppK f [v])))
     
{-
linkL takes a list of identifiers and a list of expressions, and a continuation

it will execute the continuation after having excecuted and linked the exps to the ids

-}

linkL :: [Identifier] -> [Exp] -> (Identifier, KExp) -> State CPSSt KExp     
linkL [i] [e] (v, k) = link i e (v, k) 
linkL (i:is) (e:es) (v,k) = 
  do r <- newVar
     k'<- linkL is es (v, k)
     link i e (r, k')

patToId (IdVarPat i _) = i
patToId _ = error "unexpected pattern"