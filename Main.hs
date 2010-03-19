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

module Main where

import qualified Eval as E
import qualified Compile as C
import System.Environment(getArgs)

main = do
  args <- getArgs
  loadAndEval <- return $ if length args >= 1 && (args!!0) == "e" 
                          then E.loadAndEval else C.loadAndEval
  p <- loadAndEval (args!!1) (args!!2) (if ((length args) >= 4) then True else False)
  print p