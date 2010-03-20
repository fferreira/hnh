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
         generateConstraints
       , Constraint
       )
       where

import Syntax
import TypeUtils(getType, getPatType)

import Control.Monad.State(execState, State, get, put)

type Constraint = (Type, Type, Declaration)

addConstraint :: Declaration -> Type -> Type -> State [Constraint] ()
addConstraint d t1 t2 = 
  do st <- get
     put ((t1, t2, d):st)

generateConstraints :: Program -> [Constraint]
generateConstraints (Program decls) = 
  execState (processDecls decls) []

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
     addConstraint d t t'
    where
      ts = (map getPatType ps) ++ [getType e]
      t' = foldr FunType (head ts) (tail ts)
      
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
     return ()
     
processExp d (ListExp es t) = 
  do mapM (processExp d) es
     allSameType d es
     case es of [] -> addConstraint d t (DataType "List" [VarType "a"])
                _  -> addConstraint d t (DataType "List" [(getType (head es))])

processExp d e = return ()

processAlt d caseT ts (Alternative ps e) =
  do processExp d e 
     mapM 
       (\(t, p)-> do addConstraint d caseT (getPatType p)
                     return $ addConstraint d t (getPatType p)) 
       (zip ts ps)

allSameType d (e1:e2:es) =
  do addConstraint d (getType e1) (getType e2)
     allSameType d (e2:es)
allSameType d _ = return ()    

