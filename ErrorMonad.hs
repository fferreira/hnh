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

module ErrorMonad
    (
     ErrorM(..)
    )
    where

import Text.PrettyPrint.Leijen -- requires wl-pprint installed (available in cabal)

data ErrorM a = Success a | Error String

instance Monad ErrorM where
    return t = Success t
    (Success a) >>= k = k a
    (Error msg) >>= k = Error msg
    fail msg = Error msg

instance Pretty a => Pretty (ErrorM a) where
    pretty (Error msg) = pretty "Error"<> colon <+> pretty msg
    pretty (Success a) = pretty a