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
import Text.PrettyPrint.Leijen
import System.IO

main = do
  args <- getArgs
  p <- case args of --TODO add support for the extra parameters
    (cmd:file:extra) -> case cmd of "e"  -> E.loadAndEval file "main" False
                                    "ed" -> E.loadAndEval file "main" True
                                    "c"  -> C.loadAndEval file "main" False
                                    "cd" -> C.loadAndEval file "main" True
    _ -> error "Usage: hnh [e|ed|c|cd] program"
  displayIO stdout (renderPretty 0.2 60 (p<> line))
  --print (displayS (renderPretty 0.2 80 p))