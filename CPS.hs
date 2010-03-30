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

import Data.List(find)
import Text.PrettyPrint.Leijen -- requires wl-pprint installed (available in cabal)

cpsTransform :: Program -> TransformM KExp
cpsTransform p@(Program decls) = transformOk "cps" (getMainK conts)
  where
    conts = concatMap declToK decls

declToK :: Declaration -> [(Identifier, (Identifier, KExp) -> KExp)]
declToK (PatBindDcl (IdVarPat i _{-t-}) e) = [(i, cps e)]
declToK d = []

getMainK :: [(Identifier, (Identifier, KExp) -> KExp)] -> KExp
getMainK conts  = 
  case find (\((Id n _), _) -> n == "main")  conts of
    Just (_, f) -> f (Id "algo" 42, HaltKExp) --TODO what id should it be passed?
    Nothing -> error "Unable to find function main"
  
data KExp = IFKExp Identifier KExp KExp
          | VarKExp
          | TupleKExp
          | ListKExp
          | HaltKExp -- TODO ??
          deriving (Show, Eq)
                   
instance Pretty KExp where                   
  pretty e = pretty (show e) -- TODO improve this!

cps :: Exp  -> (Identifier, KExp) -> KExp
cps e (v, k) = HaltKExp

