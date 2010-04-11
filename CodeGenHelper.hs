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

module CodeGenHelper
       (
         CVar
       , Fun(..)
       , mainWrapper
       , allocInt
       , generateFuns
       , createFun
       , callFun
       , newTuple
       , desc
       )
       where
import CPSRep

import Text.PrettyPrint.Leijen -- requires wl-pprint installed (available in cabal)

type CVar = String

data Fun = Fun String CVar String -- desc name body
         deriving Show

mainWrapper body = "\n // HNH Main\n"
                   ++ "void HNH_main (void)\n{\n"
                   ++ body ++ "\n}\n"

allocInt cvar n = cvar ++ " = alloc_int(" ++ show n ++ ");\n"
  
createFun desc name body = Fun desc name body

callFun :: String -> String
callFun fun = fun ++ "();\n"

generateFuns :: [Fun] -> String
generateFuns funs = "\n// Function Declarations\n" ++ decls 
                    ++ "\n// Function Identifiers\n" ++ globals 
                    ++ "\n// Function Value Init\n" ++ initFun
  where
    decls = concatMap genDecls funs
    genDecls (Fun desc name body) = 
      desc ++ "\n"
      ++ "void " ++ name ++ "_f (void)\n"
      ++ "{\n" ++ body ++ "\n}\n"
    globals = concatMap genGlobal funs
    genGlobal (Fun _ name _) = "value * " ++ name ++ ";\n"
    initFun = "void initFun(void)\n{\n" ++ concatMap genInit funs ++ "\n}\n"
    genInit (Fun _ name _) = name ++ "->tag = FUNCTION_VALUE;\n"
                             ++ name ++ "->function = " ++ name ++ "_f;\n"

newTuple name contents = 
  name ++ " = alloc_tuple(" ++ show (length contents) ++ ");\n"
  ++ concatMap assign (zip contents [0..])
    where
      assign(n, num) = 
        "tup_set(" ++ name ++ ", " ++ show num ++ ", " ++ n ++ ");\n"
      











desc (IfK i k1 k2) = comment $ pretty "IfK" <+> pretty i <+> pretty "k1 k2"
desc (LitK val v k t) = comment $ pretty "LitK" <+> pretty val 
                        <+> pretty v <+> pretty "k"
desc (VarK _ _ _ _) = error "Unexpected VarK"
desc (TupDK tuple n v k t) = comment $ pretty "TupDK" 
                             <+> pretty n <+> pretty v <+> pretty "k"
desc (ConDK const n v k t) = comment $ pretty "ConDK" <+> pretty const
                             <+> pretty n <+> pretty v <+> pretty "k"
desc (PrimK v k t) = comment $ pretty "PrimK" <+> pretty v
                     <+> pretty "k"
desc (AppK f params) = comment $ pretty "AppK" 
                       <+> pretty f <+> pretty params
desc (FunK fun params body k) = comment $ pretty "FunK"
                                <+> pretty fun <+> pretty params
                                <+> pretty body <+> pretty "k"
desc (TupleK ids v k) = comment $ pretty "TupleK"
                        <+> pretty ids <+> pretty v
                        <+> pretty "k"
desc (ListK ids v k) = comment $ pretty "ListK"
                        <+> pretty ids <+> pretty v
                        <+> pretty "k"
desc (SwitchK ids alts) = comment $ pretty "SwitchK"
desc HaltK = comment $ pretty "HaltK"

comment d = show (enclose 
                  (line <> pretty"/* (")
                  (pretty") */" <> line)
                  d
                  )
          
