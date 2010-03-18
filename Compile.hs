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

module Compile where --TODO add exported methods

import Parser
import Text.PrettyPrint.Leijen(Doc, Pretty, pretty)
import Syntax

import CommonTransforms(commonTransforms)
import AddIdentifiers(addIdentifiers)
import InferTypes(performTypeInference)
import GenerateConstraints --TODO debugging only

import qualified TransformMonad as T
import ErrorMonad

import EvalEnv(Env, env0, Value(..), ClosureAction(..), lookupEvalEnv, envForData)

import Control.Monad.State

import Tools
import Debug.Trace


compileTransform :: Program -> (ErrorM Program, [(String, Doc)])
compileTransform p = 
  let (res, docs)  = T.runTransform (commonTransforms p
                                     >>= addIdentifiers
                                     >>= performTypeInference
                                     >>= return)       
  in
   (res, ("original", (pretty p)):docs) -- adding the original to the list
   
runTransformations :: ErrorM Program -> (ErrorM Program, [(String, Doc)])
runTransformations (Success p) = compileTransform p
runTransformations (Error s) = error $ show (pretty s)

checkTransformation :: ErrorM Program -> Program
checkTransformation (Success program) = program
checkTransformation (Error err) = error err

loadAndEval :: String -> Name -> Bool -> IO Doc
loadAndEval file main showSteps = do contents <- readFile file
                                     preludeContents <- readFile "prelude.hnh"
                                     parsedPrelude <- return $ parseHNH "prelude.hnh" preludeContents
                                     parsed <- return $ parseHNH file contents
                                     (programRes, docs) <- return $ runTransformations (merge parsedPrelude parsed)
                                     program <- return $ checkTransformation programRes 
                                     doc <- return $ if showSteps then
                                                       T.renderSteps docs
                                                     else
                                                       compile program main
                                     return doc

merge :: ErrorM Program -> ErrorM Program -> ErrorM Program
merge (Success (Program d1)) (Success (Program d2)) = Success (Program (d1++d2))
merge e@(Error msg) _ = e
merge _ e@(Error msg) = e


compile p@(Program decls) name = pretty (generateConstraints p)

