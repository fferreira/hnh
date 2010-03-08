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
    ,TransformResult(..)
    ,transformOk
    ,transformError
    ,runTransform
    ,nullTransform
    )
    where

import Text.PrettyPrint.Leijen(Doc, Pretty, pretty)

data TransformM a = TaM [Doc] (TransformResult a)

data TransformResult a = Ok a | Failed String

runTransform :: TransformM a -> (TransformResult a, [Doc])
runTransform (TaM l r) = (r, l)

transformOk :: Pretty a => a -> TransformM a
transformOk a = (TaM [pretty a] (Ok a))

transformError :: String -> TransformM a
transformError s = fail s

instance Monad TransformM where
    return t = (TaM [] (Ok t))
    (TaM l (Ok t)) >>= k = (TaM (l++l') t')
        where (TaM l' t') = k t
    (TaM l (Failed s)) >>= k = (TaM l (Failed s))
    
    fail msg = TaM [] (Failed msg)

nullTransform ::Pretty a => a -> TransformM a
nullTransform = transformOk