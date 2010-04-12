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
module ProgramUtils
       (
         getNextIdNumExp
       , getNextIdNumKExp
       )
       where


import Syntax
import CPSRep

import Control.Monad.State(State, get, put, execState)

getNextIdNumExp :: Program -> Int
getNextIdNumExp (Program decls) = execState (mapM procDecl decls) 0

putId :: Identifier -> State Int ()
putId (Id _ num) = do st <- get
                      if num >= st then
                        put (num + 1) else return ()

procDecl :: Declaration -> State Int ()
procDecl (DataDcl t cons) = mapM procCons cons >> return ()
procDecl (PatBindDcl p e) = do procPat p
                               procExp e
procDecl d = return ()                               

procCons (IdConDcl i _) = putId i
procCons c = return ()

procPat (IdVarPat i _) = putId i
procPat (IdConPat _ ids _ _) = mapM putId ids >> return ()
procPat (IdTuplePat ids _) = mapM putId ids >> return ()
procPat p = return ()

procExp (IdVarExp i _) = putId i
procExp (IdConExp i _) = putId i
procExp (FExp e1 e2 _) = procExp e1 >> procExp e2
procExp (LambdaExp pats e _) = mapM procPat pats >> procExp e
procExp (LetExp decls e _) = mapM procDecl decls >> procExp e
procExp (IfExp e1 e2 e3 _) = procExp e1 >> procExp e2 >> procExp e3
procExp (CaseExp es alts _) = mapM procExp es
                              >> mapM procAlt alts
                              >> return ()
procExp (TupleExp es _) = mapM procExp es >> return ()                              
procExp (ListExp es _) = mapM procExp es >> return ()
procExp e = return ()

procAlt (Alternative pats e) = mapM procPat pats >> procExp e


getNextIdNumKExp :: KExp -> Int
getNextIdNumKExp k = execState (procK k) 0

procK :: KExp -> State Int ()
procK (IfK i k1 k2) =
  do putId i
     procK k1
     procK k2
     
procK (LitK _ i k) =     
  do putId i
     procK k
     
procK (VarK i i' k) = putId i >> putId i' >> procK k
procK (TupDK i _ i' k) = putId i >> putId i' >> procK k
procK (ConDK i _ i' k) = putId i >> putId i' >> procK k
procK (PrimK i k) = putId i >> procK k
procK (AppK i ids) = mapM putId ids >> putId i
procK (FunK f params body k) = putId f >> mapM putId params
                               >> procK body >> procK k
procK (TupleK ids i k) = mapM putId ids >> putId i
                         >> procK k
procK (ListK ids i k) = mapM putId ids >> putId i                         
                        >> procK k
procK (SwitchK ids alts) = mapM putId ids 
                           >> mapM procAltK alts                        
                           >> return ()
procK HaltK = return ()                          

procAltK (AltK conds k) = procK k
