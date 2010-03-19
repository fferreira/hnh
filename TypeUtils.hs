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

module TypeUtils
       (
         addType  -- TODO eliminate unused functions
       , addPatType
       , litToExp
       , assembleInfixOperator
       , getType
       , getPatType
       , resultingType
       , typeFromAlternative
       )
       where

import Syntax

import Data.List(nub)
import Text.PrettyPrint.Leijen(pretty)

-- returns the type resuling for the application of FuncType
-- or UnknownType if the type is not appropiate
resultingType :: Type -> Type
resultingType (FunType _ t) = t
resultingType _ = UnknownType

getType :: Exp -> Type
getType (VarExp _ t) = t
getType (ConExp _ t) = t
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
getType (IdConExp _ t) = t

getPatType :: Pattern -> Type
getPatType (VarPat _ t) = t
getPatType (ConPat _ _ t) = t
getPatType (TuplePat _ t) = t
getPatType (WildcardPat t) = t
getPatType (IdVarPat _ t) = t
getPatType (IdConPat _ _ _ t) = t
getPatType (IdTuplePat _ t) = t

typeFromAlternative :: Alternative -> Type
typeFromAlternative (Alternative _ e) = getType e

-- addType adds type information to an expression
addType :: Exp -> Type -> Exp
addType (VarExp n _) t = VarExp n t
addType (ConExp n _) t = ConExp n t
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
addType (IdConExp i _) t = IdConExp i t

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
litToExp val@(LiteralString _) = LitExp val (PrimType "String") -- TODO perhaps (ConType "List" (ConType "Char" []))
litToExp val@(LiteralChar _) = LitExp val (PrimType "Char")

-- assembleInfixOperator builds an infix operator structure (for fixity adaptation later)
assembleInfixOperator :: Exp -> Operator -> Exp -> Exp
assembleInfixOperator (InfixOpExp opEx1 _) op (InfixOpExp opEx2 _) = InfixOpExp 
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
