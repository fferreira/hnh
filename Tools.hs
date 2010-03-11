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

module Tools
    (
     traceVal
    ,traceP
    ,D.trace
    )
    where

import qualified Debug.Trace as D
import Text.PrettyPrint.Leijen -- requires wl-pprint installed (available in cabal)

traceVal :: Show a => a -> a
traceVal v = D.trace ("\ntrace:\n" ++ show v ++"\nEnd trace.\n") v
--traceVal v = v

traceP :: Pretty a => a -> a
traceP v = D.trace (show $ line<>pretty v<>line) v