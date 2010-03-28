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
    along with Foobar.  If not, see <http://www.gnu.org/licenses/>.
    
    Copyright 2010 Francisco Ferreira
-}
module GenerateConstraints
       (
       Constraint
       , getConstraints
       )
       where

import Syntax
import TypeUtils(getType, getPatType, getAltType, getAltPatTypes)

import Tools

import Control.Monad.State(execState, State, get, put)

type Constraint = (Type, Type, Declaration)

getConstraints :: Declaration -> [Constraint]  
getConstraints d =
  execState (processDecl d) []

addConstraint :: Declaration -> Type -> Type -> State [Constraint] ()
addConstraint d t1 t2 = 
  do st <- get
     if t1 == t2 
       then return ()
       else put ((t1, t2, d):st)

processDecls :: [Declaration] -> State [Constraint] ()
processDecls decls = do mapM processDecl decls; return ()

processDecl d@(PatBindDcl pat e) = 
  do processExp d e
     addConstraint d (getPatType pat) (getType e)
processDecl decl = return ()

processExp d (ParensExp e t) = 
  do processExp d e
     addConstraint d t (getType e)
     
processExp d (TupleExp es t) =     
  do mapM (processExp d) es
     addConstraint d t (TupleType (map getType es))
     
processExp d (FExp e1 e2 t) =     
  do processExp d e1
     processExp d e2
     addConstraint d (getType e1) (FunType (getType e2) t)
     
processExp d (LambdaExp ps e t) =
  do processExp d e
     addConstraint d t ts'
    where
      ts = map getPatType ps
      ts'= foldr FunType (getType e) ts
      
processExp d (LetExp decls e t) = 
  do processDecls decls 
     processExp d e
     addConstraint d t (getType e)
     
     
processExp d (IfExp e1 e2 e3 t) =     
  do processExp d e1 
     processExp d e2
     processExp d e3
     addConstraint d (getType e2) (getType e3)
     addConstraint d t (getType e2)
     addConstraint d (getType e1) (DataType "Bool" [])
     
processExp d (CaseExp es alts t) =
  do mapM (processExp d) es
     mapM (processAlt d t (map getType es)) alts
     -- all the exps in alts must have the same type
     allSameType d (map getAltType alts) 
     -- the alt exps should be the same type as the case
     addConstraint d t (getAltType (head alts)) 
     -- all the patterns in the alts should have the same type
     mapM (allSameType d) (rows2cols (map getAltPatTypes alts))
     -- the exps and the patterns should have the same type
     mapM (allSameType d) (rows2cols [(map getType es)
                                     ,(getAltPatTypes (head alts))])
     return ()
     
processExp d (ListExp es t) = 
  do mapM (processExp d) es
     allSameType d (map getType es)
     case es of [] -> addConstraint d t (DataType "List" [VarType "a"])
                _  -> addConstraint d t (DataType "List" [(getType (head es))])

processExp d e = return ()

processAlt d caseT ts (Alternative ps e) =
  do processExp d e 
     mapM 
       (\(t, p)-> do {-addConstraint d caseT (getPatType p)-}
                     return $ addConstraint d t (getPatType p)) 
       (zip ts ps)

allSameType d (t1:t2:ts) =
  do addConstraint d t1 t2
     allSameType d (t2:ts)
allSameType d _ = return ()    



{-
rows2cols takes some lists, and returns a list of 
the list of the first element of each list and the
list of the second element of each list etc...
-}

rows2cols :: [[ a ]] -> [[ a ]]
rows2cols t = if (length . head $ t) > 1 then 
                (map head t):rows2cols (map tail t)
              else
                [concat t]