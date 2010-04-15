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

module Compile where --TODO add exported methods

import Parser

import Syntax

import CommonTransforms(commonTransforms)
import OneVarLambda(oneVarLambda)
import AddIdentifiers(addIdentifiers)
import ProgToLet(progToLet)
import InferTypes(performTypeInference)
import CPS(cpsTransform)
import RemoveVarK(removeVarK)
import Closure(closureConversion)
import CodeGen(codeGen)
import InitialProgram(buildInitialProgram)

import qualified TransformMonad as T
import ErrorMonad

import Control.Monad.State
import Text.PrettyPrint.Leijen{-(Doc, Pretty, pretty)-}

-- compileTransform :: Program -> (ErrorM Program, [(String, Doc)])
compileTransform prog = 
  let (res, docs)  = T.runTransform (commonTransforms p
                                     >>= oneVarLambda
                                     >>= addIdentifiers
                                     >>= performTypeInference
                                     >>= progToLet
                                     >>= cpsTransform
                                     >>= removeVarK
                                     >>= closureConversion
                                     >>= codeGen
                                     >>= return)
      p = buildInitialProgram prog
  in
   (res, ("original", (pretty p)):docs) -- adding the original to the list
   
-- runTransformations :: ErrorM Program -> (ErrorM Program, [(String, Doc)])
runTransformations (Success p) = compileTransform p
runTransformations (Error s) = error $ show (pretty s)

-- checkTransformation :: ErrorM Program -> Program
checkTransformation (Success program) = program
checkTransformation (Error err) = error err

loadAndEval :: String -> Name -> Bool -> IO Doc
loadAndEval file main showSteps = 
  do contents <- readFile file
     preludeContents <- readFile "prelude.hnh"
     parsedPrelude <- return $ parseHNH "prelude.hnh" preludeContents
     parsed <- return $ parseHNH file contents
     (programRes, docs) <- return $ 
                           runTransformations (merge parsedPrelude parsed)
     program <- return $ checkTransformation programRes 
     writeFile "code.c" program
     doc <- return $ if showSteps then
                       T.renderSteps docs
                     else
                       compile program main
     return doc

-- merge :: ErrorM Program -> ErrorM Program -> ErrorM Program
merge (Success (Program d1)) (Success (Program d2)) = Success (Program (d1++d2))
merge e@(Error msg) _ = e
merge _ e@(Error msg) = e


compile p name = pretty p

prettify (t1, t2, d) = pretty t1 
                       <+> pretty "=" 
                       <+> pretty t2 
                       <> line <> pretty d <> line