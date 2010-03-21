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
module PolyType
       (
         transformType
       , initialPoly
       , getNext
       )
       where

import Syntax

import Control.Monad.State(evalState, State, put, get)

data PolySt = PolySt {
  nextMeta :: Int
  , typeEnv :: [(Type, Type)] -- replace the first by the second 
  }
              
initialPoly n = PolySt n []

getNext (PolySt next _) = next

getNewMeta :: State PolySt Type              
getNewMeta = 
  do PolySt next env <- get
     put $ PolySt (next + 1) env
     return $ MetaType next
     
addNewMeta :: Type -> State PolySt Type     
addNewMeta t =
  do PolySt next env <- get
     put $ PolySt (next + 1) ((t, MetaType (next + 1)): env)
     return $ MetaType (next + 1)
     
transformType :: Type -> State PolySt Type     
transformType t@(PrimType _) = return t
transformType t@(MetaType _) = return t
transformType UnknownType = getNewMeta
transformType t@(VarType _) = 
  do PolySt _ env <- get
     case lookup t env of Just t' -> return t'
                          Nothing -> addNewMeta t
                          
transformType (FunType t1 t2) =
  do t1' <- transformType t1
     t2' <- transformType t2
     return $ FunType t1' t2'
     
transformType (TupleType ts) =     
  do ts' <- mapM transformType ts
     return $ TupleType ts'
     
transformType (DataType n ts) =     
  do ts' <- mapM transformType ts
     return $ DataType n ts'
  

