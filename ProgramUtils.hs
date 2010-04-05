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
         getNextIdNum
       )
       where


import Syntax

import Control.Monad.State(State, get, put, execState)

getNextIdNum :: Program -> Int
getNextIdNum (Program decls) = execState (mapM procDecl decls) 0

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