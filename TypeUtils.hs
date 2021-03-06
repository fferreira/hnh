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

module TypeUtils
       (
         addType  -- TODO eliminate unused functions
       , addPatType
       , litToExp
       , assembleInfixOperator
       , getType
       , getPatType
       , resultingType
       , getAltType
       , getAltPatTypes
       , getDataTypes
       , DataType
       , getConstType
       , getConstTypeParams
       , isVarDecl
       , getTupleType
       , constToFun
       )
       where

import Syntax

import Data.List(find)

-- returns the type resuling for the application of FuncType
-- or UnknownType if the type is not appropiate
resultingType :: Type -> Type
resultingType (FunType _ t) = t
resultingType _ = UnknownType

getType :: Exp -> Type
getType (VarExp _ t) = t
getType (ConExp _ _ t) = t
getType (LitExp _ t) = t
getType (InfixOpExp _ t) = t
getType (FExp _ _ t) = t
getType (MinusExp _ t) = t
getType (MinusFloatExp _ t) = t
getType (LambdaExp _ _ t) = t
getType (LetExp _ _ t) = t
getType (IfExp _ _ _ t) = t
getType (CaseExp _ _ t) = t
getType (ParensExp _ t) = t
getType (TupleExp _ t) = t
getType (ListExp _ t) = t
getType (IdVarExp _ t) = t
getType (IdConExp _ _ t) = t
getType (Prim _ _ t) = t
getType (IdPrim _ _ t) = t

getPatType :: Pattern -> Type
getPatType (VarPat _ t) = t
getPatType (ConPat _ _ t) = t
getPatType (TuplePat _ t) = t
getPatType (WildcardPat t) = t
getPatType (IdVarPat _ t) = t
getPatType (IdConPat _ _ _ t) = t
getPatType (IdTuplePat _ t) = t

getAltType :: Alternative -> Type
getAltType (Alternative _ e) = getType e

getAltPatTypes :: Alternative -> [Type]
getAltPatTypes (Alternative ps _) = map getPatType ps

-- addType adds type information to an expression
addType :: Exp -> Type -> Exp
addType (VarExp n _) t = VarExp n t
addType (ConExp n params _) t = ConExp n params t
addType (LitExp v _) t = LitExp v t
addType (InfixOpExp e _) t = InfixOpExp e t
addType (FExp e e' _) t = FExp e e' t
addType (MinusExp e _) t = MinusExp e t
addType (MinusFloatExp e _) t = MinusFloatExp e t
addType (LambdaExp p e _) t = LambdaExp p e t
addType (LetExp d e _) t = LetExp d e t
addType (IfExp e1  e2 e3 _) t = IfExp e1 e2 e3 t
addType (CaseExp e a _) t = CaseExp e a t
addType (ParensExp e _) t = ParensExp e t
addType (TupleExp e _) t = TupleExp e t
addType (ListExp e _) t = ListExp e t
addType (IdVarExp i _) t = IdVarExp i t
addType (IdConExp i params _) t = IdConExp i params t
addType (Prim n params _) t = Prim n params t
addType (IdPrim n params _) t = IdPrim n params t

addPatType :: Pattern -> Type -> Pattern
addPatType (VarPat n _ ) t = (VarPat n t)
addPatType (ConPat n ns _) t = (ConPat n ns t)
addPatType (TuplePat ns _) t = (TuplePat ns t)
addPatType (WildcardPat _) t = (WildcardPat t)
addPatType (IdVarPat i _) t = (IdVarPat i t)
addPatType (IdConPat n ids ts _) t = (IdConPat n ids ts t)
addPatType (IdTuplePat ids _) t = (IdTuplePat ids t)

-- litToExp creates an Expresion from a literal (with the right type)
litToExp :: LiteralValue -> Exp
litToExp val@(LiteralInt _) = LitExp val (PrimType "Int")
litToExp val@(LiteralFloat _) = LitExp val (PrimType "Float")
litToExp val@(LiteralString _) = LitExp val (PrimType "String")
litToExp val@(LiteralChar _) = LitExp val (PrimType "Char")

-- assembleInfixOperator builds an infix operator structure 
-- (for fixity adaptation later)
assembleInfixOperator :: Exp -> Operator -> Exp -> Exp
assembleInfixOperator (InfixOpExp opEx1 _) op (InfixOpExp opEx2 _) = 
  InfixOpExp 
  (Op op opEx1 opEx2)
  UnknownType
assembleInfixOperator (InfixOpExp opEx _) op e = InfixOpExp
                                                 (Op op opEx (LeafExp e))
                                                 UnknownType
assembleInfixOperator e op (InfixOpExp opEx _) = InfixOpExp
                                                 (Op op (LeafExp e) opEx)
                                                 UnknownType
assembleInfixOperator e1 op e2 = InfixOpExp
                                 (Op op (LeafExp e1) (LeafExp e2))
                                 UnknownType


-- Constructors to Functions
constToFun :: [Declaration] -> [Declaration]
constToFun decls = concatMap procDecl decls
  where
    procDecl (DataDcl t cons) = map (procCons t) cons
    procDecl d = []
    
    procCons t (ConDcl n []) = (PatBindDcl (VarPat n t) (ConExp n [] t))
    procCons t (ConDcl n ts) = (PatBindDcl 
                                (VarPat n (toFun (ts++[t]))) 
                                (LambdaExp 
                                 pats 
                                 (ConExp n params t) 
                                 UnknownType))
      where
        pats = map (\n -> (VarPat n UnknownType)) params
        params = map (\c->[c]) (take (length ts) varNames)
    
    
    toFun (t:[]) = t
    toFun (t:ts) = FunType t (toFun ts)
    varNames = "abcdefghijkalmnopqrstuvwxyz"    



type DataType = (Type, [Constructor])

getDataTypes :: Program -> [DataType]
getDataTypes (Program decls) = map getDataT (filter isDataT decls)
    where
      isDataT (DataDcl _ _) = True
      isDataT _ = False
      
      getDataT (DataDcl t cs) = (t,cs)
    
getConstType :: [DataType] -> Name -> Maybe Type
getConstType dts n =
  find isData dts >>= return . fst
    where
      isData (_, cons) = case find isCon cons of Just _ -> True
                                                 Nothing -> False
      isCon (ConDcl n' _) = n == n'

getConstTypeParams :: [DataType] -> Name -> Maybe [Type]
getConstTypeParams dts n =
  find isCon cons >>= return . getConType
    where
      cons = concat . snd . unzip $ dts
      isCon (ConDcl n' _) = n == n'
      getConType (ConDcl _ ts) = ts
    
isVarDecl :: Declaration -> Bool
isVarDecl (PatBindDcl _ _) = True
isVarDecl _ = False    

getTupleType :: Type -> Int -> Type
getTupleType (TupleType ts) n = ts!!n
getTupleType t n = UnknownType
