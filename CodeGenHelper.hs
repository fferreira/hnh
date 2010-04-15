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
       , fileHeader
       , mainWrapper
       , allocInt
       , generateFuns
       , createFun
       , getAssign
       , callFun
       , newTuple
       , getTuple
       , getData
       , genHalt
       , genPrim
       , genCon
       , genIf
       , desc
       )
       where
import CPSRep

import Text.PrettyPrint.Leijen -- requires wl-pprint installed
import Data.List(intersperse)

type CVar = String

data Fun = Fun String CVar [CVar] String -- desc name params body
         deriving Show
                  
intrinsics :: [(Name, CVar)]                  
intrinsics = 
  [
    ("+", "int_add")
  , ("-", "int_sub")
  , ("*", "int_mul")
  , ("/", "int_div")
  , ("~", "int_neg")
  , ("==", "int_eq")
  , ("<", "int_lt")
  , (">", "int_gt")
  ]

getIntrinsic :: Name -> CVar
getIntrinsic n = 
  case lookup n intrinsics of
    Nothing -> error ("Unsupported intrinsic function: " ++ n)
    Just f -> f

fileHeader = "#include \"runtime.h\"\n"
             ++ "#include \"intrinsic.h\"\n"
             ++ "#include <assert.h>\n"


mainWrapper body = "\n // HNH Main (param not used)\n"
                   ++ "call_k HNH_main (value * param)\n{\n"
                   ++ body ++ "\n}\n"

allocInt cvar n = 
  "value * " ++ cvar ++ " = alloc_int(" ++ show n ++ ");\n"
  
createFun desc name params body = Fun desc name params body

getAssign v ov = 
    "value * " ++ v ++ " = " ++ ov ++ ";\n"

callFun fun params var =      
  "value * " ++ var ++ " = alloc_tuple(" ++ show (length params) ++ ");\n"
  ++ pack
  ++ "assert(" ++ fun ++ "->tag == FUNCTION_VALUE);\n"
  ++ "\nreturn ret_val(" ++ fun ++ "->function, " ++ var ++ ");"
  where
    pack = concatMap (\(v, n) -> 
                       "tup_set(" ++ var 
                       ++ ", " ++ show n 
                       ++ ", " ++ v ++ ");\n")
           (zip params [0..])
  
  
generateFuns :: [Fun] -> String
generateFuns funs = "\n// Function Identifiers\n" ++ globals 
                    ++ "\n// Function Declarations\n" ++ decls 
                    ++ "\n// Function Value Init\n" ++ initFun
  where
    decls = concatMap genDecls funs
    genDecls (Fun desc name params body) =
      desc ++ "\n"
      ++ "call_k " ++ name ++ "_f (value * params)\n"
      ++ "{\n" ++ extractParams params ++ body ++ "\n}\n"
    extractParams params = concatMap 
                           (\(p,n) -> 
                             "value * "++ p 
                             ++ " = tup_get(params, " ++ show n ++ ");\n")
                           (zip params [0..])
    globals = concatMap genGlobal funs
    genGlobal (Fun _ name _  _) = "value * " ++ name ++ ";\n"
    initFun = "void init_fun(void)\n{\n" ++ concatMap genInit funs ++ "\n}\n"
    genInit (Fun _ name _ _) = name ++ " = alloc_function(" ++ name ++ "_f);\n"  

newTuple name contents = 
  "value * "++ name ++ " = alloc_tuple(" ++ show (length contents) ++ ");\n"
  ++ concatMap assign (zip contents [0..])
    where
      assign(n, num) = 
        "tup_set(" ++ name ++ ", " ++ show num ++ ", " ++ n ++ ");\n"
      
getTuple tuple elem var =
  "value * " ++ var ++ " = tup_get(" ++ tuple ++ ", " ++ show elem ++ ");\n"

getData con elem var =
  "value * " ++ var ++ " = data_get(" ++ con ++ ", " ++ show elem ++ ");\n"

genHalt v = "halt_continuation(" ++ v ++");\n"

genPrim n params var = "value * " ++ var
                       ++ " = " ++ getIntrinsic n
                       ++ "("
                       ++ concat (intersperse "," params)
                       ++ ");"
                      
genCon n params var = "value * " ++ var
                      ++ " = alloc_data(\"" ++ n ++ "\", " 
                      ++ show (length var) ++ ");\n"
                      ++ concatMap 
                      (\(p, n) -> 
                        "data_set(" ++ var ++ ", " 
                        ++ show n ++ ", " ++ p ++");\n") 
                      (zip params [1..])

genIf cond kthen kelse = 
  -- "if (strcmp(" ++ cond ++ "->data_value.constructor, \"True\") == 0){\n"
  "if (is_constructor(" ++ cond ++ ", \"True\")){\n"
  ++ kthen ++ "\n } else {\n"
  ++ kelse ++ "\n}\n"
  

desc (IfK i k1 k2) = comment $ pretty "IfK" <+> pretty i <+> pretty "k1 k2"
desc (LitK val v k) = comment $ pretty "LitK" <+> pretty val 
                      <+> pretty v <+> pretty "k"
desc (VarK i i' k) = comment $ pretty "VarK" <+> pretty i
                     <+> pretty i' <+> pretty "k"
desc (TupDK tuple n v k) = comment $ pretty "TupDK" <+> pretty tuple 
                           <+> pretty n <+> pretty v <+> pretty "k"
desc (ConDK const n v k) = comment $ pretty "ConDK" <+> pretty const
                           <+> pretty n <+> pretty v <+> pretty "k"
desc (PrimK n params v k) = comment $ pretty "PrimK" <+> pretty n
                          <+> pretty params <+> pretty v <+> pretty "k"
desc (ConK n params v k) = comment $ pretty "ConK" <+> pretty n       
                           <+> pretty params <+> pretty v <+> pretty "k"
desc (AppK f params) = comment $ pretty "AppK" 
                       <+> pretty f <+> pretty params
desc (FunK fun params body k) = comment $ pretty "FunK"
                                <+> pretty fun <+> pretty params
                                <+>  pretty "body" <+> pretty "k"
desc (TupleK ids v k) = comment $ pretty "TupleK"
                        <+> pretty ids <+> pretty v
                        <+> pretty "k"
desc (ListK ids v k) = comment $ pretty "ListK"
                        <+> pretty ids <+> pretty v
                        <+> pretty "k"
desc (SwitchK ids alts) = comment $ pretty "SwitchK"
desc (HaltK i)= comment $ pretty "HaltK" <+> pretty i

comment d = show (enclose 
                  (line <> pretty"/* (")
                  (pretty") */" <> line)
                  d
                  )
          
