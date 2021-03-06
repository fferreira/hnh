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
import CPSRep
import TransformMonad (TransformM, transformOk)
import TypeUtils(getTupleType)
import ProgramUtils(getNextIdNumExp)
import BuiltIn(resultId)

import Control.Monad.State(evalState, State, get, put)
import Data.List(find)


import Tools

cpsTransform :: Program -> TransformM KExp
cpsTransform p@(Program decls) = transformOk "cps" 
                                 (getMainK conts)
  where
    conts = concatMap (declToK (initialSt p)) decls

declToK :: CPSSt -> Declaration -> [(Identifier, (Identifier, KExp) -> KExp)]
declToK ini (PatBindDcl (IdVarPat i _{-t-}) e) = [(i, cpsConv ini e)]
declToK ini d = []

getMainK :: [(Identifier, (Identifier, KExp) -> KExp)] -> KExp
getMainK conts  = 
  case find (\((Id n _), _) -> n == "main") conts of
    -- reusltId has the result of the program
    Just (_, f) -> f (resultId, HaltK resultId) 
    Nothing -> error "Unable to find function main"
  
type CPSSt = Int

cpsConv :: CPSSt -> Exp  -> (Identifier, KExp) -> KExp
cpsConv ini e (v, k) = evalState (cps e (v, k)) ini

initialSt prog = getNextIdNumExp prog

newVar :: State CPSSt Identifier
newVar = do next <- get
            put (next + 1)
            return (Id "cont" next)
            
newVars :: Int -> State CPSSt [Identifier]           
newVars 0 = return []
newVars 1 = do v <- newVar; return [v]
newVars n = do v <- newVar
               rest <- newVars (n-1) 
               return (v:rest)

{-
cps takes an Exp, and id and KExp
and returns a KExp' produced from Exp, that will have KExp
as continuation, and where its value will be 
-}
cps :: Exp  -> (Identifier, KExp) -> State CPSSt KExp
cps (LitExp val t) (v, k) = return $ LitK val v k

cps (FExp e1 e2 t) (v, k) = 
  do xk <- newVar
     f <- newVar -- the function
     x <- newVar -- its parameter
     ek <- cps e2 (x, FunK xk [v] k (AppK f [x, xk]))
     cps e1 (f, ek)
     
cps (LambdaExp pats e t) (v, k) = 
  do xk <- newVar
     res <- newVar
     ke <- cps e (res, AppK xk [res])
     return (FunK v ((map patToId pats) ++ [xk]) ke k)
     
     
cps (LetExp decls e t) (v, k) =
  do n <- newVar 
     ke <- cps e (v, k)
     linkL ids exps ke
       where
         (ids, exps) = unzip $ map (\(PatBindDcl (IdVarPat i _) e) -> (i, e)) decls

cps (IfExp ec e1 e2 t) (v, k) = 
  do xc <- newVar -- result of the condition
     f <- newVar  -- variable that will hold the continuation function
     r1 <- newVar -- result of the then branch
     r2 <- newVar -- result of the else branch
     e1' <- (cps e1 (r1, AppK f [r1]))
     e2' <- (cps e2 (r2, AppK f [r2]))
     
     ec' <- cps ec (xc, IfK xc e1' e2')
     
     return $ FunK f [v] k ec'

cps (CaseExp es alts t) (v, k) = 
  do ids <- newVars(length es)
     v' <- newVar
     conds <- mapM (cpsAlt ids (v,k))  alts
     linkL ids es (SwitchK ids conds)
     
     
cps (ParensExp e t) (v, k) = cps e (v,k)
cps (TupleExp es t) (v, k) = 
  do ids <- newVars (length es)
     linkL ids es (TupleK ids v k)
cps (ListExp es t) (v, k) = 
  do ids <- newVars (length es)
     linkL ids es (ListK ids v k)
  
cps (IdVarExp i t) (v, k) = return $ VarK i v k
cps (IdConExp n params t) (v, k) = return $ ConK n params v k
cps (IdPrim n params t) (v, k) = return (PrimK n params v k)

cps (VarExp _ _) (_, _)        = error "Unexpected VarExp"
cps (ConExp _ _ _) (_, _)      = error "Unexpected ConExp"
cps (InfixOpExp _ _) (_, _)    = error "Unexpected InfixOpExp"
cps (MinusFloatExp _ _) (_, _) = error "Unexpected MinusFloatExp"
cps (MinusExp _ _) (_, _)      = error "Unexpected MinusExp"
cps (Prim _ _ _) (_, _)        = error "Unexpected MinusExp"

cpsAlt :: [Identifier] -> (Identifier, KExp) -> Alternative -> State CPSSt AltK
cpsAlt caseIds (v,k) (Alternative pats e) = 
  do ke <- cps e (v, k)
     v' <- newVar
     ke' <- addLinks caseIds pats (v', ke)
     return $ AltK (map patToCond pats) ke'

patToCond :: Pattern -> CondK
patToCond (WildcardPat _) = WildK
patToCond (IdVarPat _ _) = WildK
patToCond (IdTuplePat _ _) = WildK
patToCond (IdConPat n _ _ _) = CondK n

addLinks :: [Identifier] -> [Pattern] -> (Identifier, KExp) -> State CPSSt KExp
addLinks [i] [p] (v, k) = return $ addPatVars i p (v,k)
addLinks (i:is) (p:ps) (v, k) = 
  do v' <- newVar
     k' <- addLinks is ps (v,k)
     return $ addPatVars i p (v', k')


addPatVars :: Identifier -> Pattern -> (Identifier, KExp) -> KExp
addPatVars i (WildcardPat _) (v, k) = k
addPatVars i (IdVarPat var t) (v, k) = (VarK i var k) 
addPatVars i (IdConPat name [] _ _) (v, k) = k
addPatVars i (IdConPat name ids types t) (v, k) = conv i idsnum k
  where
    idsnum = zip ids [0..] -- list of pairs (id, num)

    conv :: Identifier -> [(Identifier, Int)] -> KExp -> KExp
    conv i [(i', n)] k = ConDK i n i' k 
    conv i ((i', n):is) k = ConDK i n i' (conv i is k) 

addPatVars i (IdTuplePat ids t) (v, k) = conv i idsnum k
  where
    idsnum = zip ids [0..] -- list of pairs (id, num)

    conv :: Identifier -> [(Identifier, Int)] -> KExp -> KExp
    conv i [(i', n)] k = TupDK i n i' k 
    conv i ((i', n):is) k = TupDK i n i' (conv i is k) 

{-
linkL takes a list of identifiers and a list of expressions, and a continuation
it will execute the continuation after having excecuted and linked the exps to the ids
-}

linkL :: [Identifier] -> [Exp] -> KExp -> State CPSSt KExp     
linkL  _ []  k = return k
linkL [i] [e] k = cps e (i, k)
linkL (i:is) (e:es) k = 
  do k'<- linkL is es k
     cps e (i, k')

patToId (IdVarPat i _) = i
patToId _ = error "unexpected pattern"