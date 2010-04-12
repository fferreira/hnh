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

module CodeGen
       (
         codeGen
       )
       where
  

import CPSRep
import CodeGenHelper
import TransformMonad (TransformM, transformOk)

import Control.Monad.State(State, put, get, runState)

codeGen :: KExp -> TransformM String
codeGen k = transformOk 
            "codeGen" 
            (generateFuns funs ++ "\n" ++ mainWrapper main)
  where
    (main, CodeGenSt _ _ funs) = (runState (procK k) initialSt)


data CodeGenSt = 
  -- next var, Identifier to C-var name dict, functiosn
  CodeGenSt Int [(Identifier, CVar)] [Fun]

initialSt = CodeGenSt 1 [((Id "end" 0),"RES" )] []

newCVar :: Identifier -> State CodeGenSt CVar
newCVar (Id "end" 0) = return "RES" -- this is a special name
newCVar i = 
  do CodeGenSt n dict fs <- get
     put (CodeGenSt (n+1) ((i, cvar n):dict) fs)
     return (cvar n)
  where
    cvar :: Int -> String
    cvar n = "var" ++ show n
    
getCVar :: Identifier -> State CodeGenSt CVar
getCVar i = 
  do CodeGenSt _ dict _<- get
     case lookup i dict of 
       Nothing -> error ("Error:" ++ show i ++ "not found!")
       Just v -> return v
       
addFun :: Fun -> State CodeGenSt ()       
addFun fun = 
  do CodeGenSt n d fs <- get
     put (CodeGenSt n d (fs++[fun]))
                           
procK :: KExp -> State CodeGenSt String
procK ke@(IfK i k1 k2) = return (desc ke)

procK ke@(LitK (LiteralInt n) v k) = 
  do vc <- newCVar v
     code <- procK k
     return (desc ke ++ allocInt vc n  ++ code)
           
procK ke@(LitK val v k) = error "Unsupported literal" --TODO complete

procK ke@(VarK _ _ _) = error "Unexpected VarK"

procK ke@(TupDK tuple n v k) = 
  do vc <- newCVar v
     tuplec <- getCVar tuple
     code <- procK k
     return (desc ke ++ getTuple tuplec n vc  ++ code)

procK ke@(ConDK const n v k) = return (desc ke)
procK ke@(PrimK v k) = 
  do code <- procK k
     return ({-desc ke ++-} code)

procK ke@(AppK f params) = 
  do fc <- getCVar f
     return (desc ke ++ callFun fc) -- TODO complete use params

procK ke@(FunK fun params body k) = 
  do cfun <- newCVar fun
     body' <- procK body
     addFun (createFun (desc ke) cfun body') --TODO complete use params
     code <- procK k
     return code
     
procK ke@(TupleK ids v k) = 
  do idsc <- mapM getCVar ids
     vc <- newCVar v
     code <- procK k
     return (desc ke ++ newTuple vc idsc  ++ code)
procK ke@(ListK ids v k) = return (desc ke)
procK ke@(SwitchK ids alts) = return (desc ke)
procK ke@(HaltK) = return (desc ke)


