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

module TransformMonad
    (
     TransformM
    ,transformOk
    ,transformError
    ,runTransform
    ,nullTransform
    ,renderSteps
    )
    where

import ErrorMonad(ErrorM(..))

import Data.List(intersperse)
import Text.PrettyPrint.Leijen

data TransformM a = TaM [(String, Doc)] (ErrorM a)

runTransform :: TransformM a -> (ErrorM a, [(String , Doc)])
runTransform (TaM l r) = (r, l)

transformOk :: Pretty a => String -> a -> TransformM a
transformOk s a = (TaM [(s, pretty a)] (Success a))

transformError :: String -> TransformM a
transformError s = fail s

instance Monad TransformM where
    return t = (TaM [] (Success t))
    (TaM l (Success t)) >>= k = (TaM (l++l') t')
        where (TaM l' t') = k t
    (TaM l (Error s)) >>= k = (TaM l (Error s))
    
    fail msg = TaM [] (Error msg)

nullTransform ::Pretty a => a -> TransformM a
nullTransform = transformOk "nullTransform"

renderSteps :: [(String, Doc)] -> Doc
renderSteps steps = 
  let
    (titles, docs) = unzip steps
    titleDocs = map (\s -> line 
                           <> pretty ">>>" 
                           <+> pretty s 
                           <+> pretty "transform:" <> line) titles
    result = vsep $ combine titleDocs docs
    epigraph = pretty "Number of phases:" <> pretty ((length docs) - 1)
  in
   result <> line <> epigraph
   
      
combine :: [Doc] -> [Doc] -> [Doc]   
combine (a:as) (b:bs) = a:b:(combine as bs)
combine [] b = b 
combine a [] = a
