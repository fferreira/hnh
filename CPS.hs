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
import TransformMonad (TransformM, transformOk)

import Control.Monad.State(evalState, State, get, put)
import Data.List(find)
import Text.PrettyPrint.Leijen -- requires wl-pprint installed (available in cabal)

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
  

data KExp = IfK Identifier KExp KExp Type
          | LitK LiteralValue KExp Type
          | AppK Identifier [Identifier]
-- params, function body, prev res, after definition, type            
          | FunK [Identifier] KExp Identifier KExp Type
          | LetK [Identifier] [KExp] KExp
          | TupleK
          | ListK
          | HaltK
          deriving (Show, Eq)
                   
instance Pretty KExp where                   
  pretty e = pretty (show e) -- TODO improve this!

type CPSSt = Int

cpsConv :: Exp  -> (Identifier, KExp) -> KExp
cpsConv e (v, k) = evalState (cps e (v, k)) initialSt

initialSt = 1000 -- TODO put the correct number here

newVar :: State CPSSt Identifier
newVar = do next <- get
            put (next + 1)
            return (Id "cont" next)

cps :: Exp  -> (Identifier, KExp) -> State CPSSt KExp
cps (LitExp val t) (v, k) = return $ LitK val k t -- TODO is this correct?
cps (FExp e1 e2 t) (v, k) = return $ k
     
cps (LambdaExp pats e t) (v, k) = 
  do xk <- newVar
     res <- newVar
     ke <- cps e (res, AppK xk [res])
     return (FunK (map patToId pats) ke v k t)
     
     
cps (LetExp decls e t) (v, k) =
  do exit <- newVar 
     ke <- cps e (exit, k)
     linkL ids exps (v, ke)
       where
         (ids, exps) = unzip $ map (\(PatBindDcl (IdVarPat i _) e) -> (i, e)) decls


  
  
cps (IfExp ec e1 e2 t) (v, k) =
  do xc <- newVar
     k1 <- (cps e1 (v, k))
     k2 <- (cps e2 (v, k))
     cps ec (xc, IfK xc k1 k2 t)

cps (CaseExp es alts t) (v, k) = return $ k
cps (ParensExp e t) (v, k) = cps e (v,k)
cps (TupleExp es t) (v, k) = return $ k
cps (ListExp es t) (v, k) = return $ k
cps (IdVarExp i t) (v, k) = 
  do f <- newVar
     return $ FunK [v] k f (AppK f [i]) t -- TODO WRONG TYPE! --TODO what about VarK?
cps (IdConExp i t) (v, k) = return $ k

cps (VarExp _ _) (_, _)        = error "Unexpected VarExp"
cps (ConExp _ _) (_, _)        = error "Unexpected ConExp"
cps (InfixOpExp _ _) (_, _)    = error "Unexpected InfixOpExp"
cps (MinusFloatExp _ _) (_, _) = error "Unexpected MinusFloatExp"
cps (MinusExp _ _) (_, _)      = error "Unexpected MinusExp"

cpsL :: [Exp] -> [Identifier]
cpsL = undefined


{-
 link takes an identifier, a variable and a continuation
 it executes the continuation where the variable is bound to the expresion
-}
link :: Identifier -> Exp -> (Identifier, KExp)  -> State CPSSt KExp --TODO add (v, k)
link i e (v, k)=
  do --fke <- cps e 
     i' <- newVar
     f <- newVar
     x <- newVar
     cps e (v, (FunK [i'] k f  (AppK f [x]) UnknownType)) --TODO type?
     
linkL :: [Identifier] -> [Exp] -> (Identifier, KExp) -> State CPSSt KExp     
linkL ids exps (v, k) = undefined

patToId (IdVarPat i _) = i
patToId _ = error "unexpected pattern"