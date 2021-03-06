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
            (fileHeader ++ generateFuns funs ++ "\n" 
             ++ mainWrapper main)
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
       Nothing -> error ("CodeGen Error: " ++ show i ++ " not found!")
       Just v -> return v
       
interVar :: State CodeGenSt CVar -- intermediate var       
interVar = do CodeGenSt n dict funs <- get
              put (CodeGenSt (n+1) dict funs)
              return ("inter" ++ show n)
       
addFun :: Fun -> State CodeGenSt ()       
addFun fun = 
  do CodeGenSt n d fs <- get
     put (CodeGenSt n d (fs++[fun]))
                           
procK :: KExp -> State CodeGenSt String
procK ke@(IfK i k1 k2) = 
  do ic <- getCVar i
     kthen <- procK k1
     kelse <- procK k2
     return (desc ke ++ genIf ic kthen kelse)

procK ke@(LitK (LiteralInt n) v k) = 
  do vc <- newCVar v
     code <- procK k
     return (desc ke ++ allocInt vc n  ++ code)
procK ke@(LitK (LiteralChar c) v k) =
  do vc <- newCVar v
     code <- procK k
     return (desc ke ++ allocChar vc c ++ code)
     
procK ke@(LitK (LiteralFloat f) v k) =     
  do vc <- newCVar v
     code <- procK k
     return (desc ke ++ allocFloat vc f ++ code)
     
procK ke@(LitK val v k) = error "Unsupported literal" --TODO complete

procK ke@(VarK old new k) = 
  do oldc <- getCVar old
     newc <- newCVar new
     code <- procK k
     return (desc ke ++ getAssign newc oldc ++ code)

procK ke@(TupDK tuple n v k) = 
  do vc <- newCVar v
     tuplec <- getCVar tuple
     code <- procK k
     return (desc ke ++ getTuple tuplec n vc  ++ code)

procK ke@(ConDK const n v k) = 
  do vc <- newCVar v
     constc <- getCVar const
     code <- procK k
     return (desc ke ++ getData constc n vc ++ code)
     
procK ke@(PrimK n params v k) = 
  do vc <- newCVar v 
     params' <- mapM getCVar params
     code <- procK k
     return (desc ke ++ genPrim n params' vc ++ code)
     
procK ke@(ConK n params v k) =
  do vc <- newCVar v 
     params' <- mapM getCVar params
     code <- procK k
     return (desc ke ++ genCon n params' vc ++ code)

procK ke@(AppK f params) = 
  do fc <- getCVar f
     params' <- mapM getCVar params
     var <- interVar
     return (desc ke ++ callFun fc params' var)

procK ke@(FunK fun params body k) = 
  do cfun <- newCVar fun
     params' <- mapM (\p -> newCVar p) params
     body' <- procK body
     addFun (createFun (desc ke) cfun params'  body')
     code <- procK k
     return code
     
procK ke@(TupleK ids v k) = 
  do idsc <- mapM getCVar ids
     vc <- newCVar v
     code <- procK k
     return (desc ke ++ newTuple vc idsc  ++ code)
     
procK ke@(ListK ids v k) = 
  do idsc <- mapM getCVar ids
     vc <- newCVar v
     code <- procK k
     return (desc ke ++ genList idsc vc  ++ code)
  
  
procK ke@(SwitchK ids alts) = 
  do idsc <- mapM getCVar ids
     altsc <- mapM procAltK alts
     return (desc ke ++ genSwitchK idsc altsc) 
       where
         procAltK (AltK conds k) = 
           do code <- procK k
              return (conds, code)

procK ke@(HaltK i) = 
  do ic <- getCVar i
     return (desc ke ++ genHalt ic)


