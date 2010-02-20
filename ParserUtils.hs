module ParserUtils
    (
     addType
    ,litToExp
    ,assembleInfixOperator
    ,checkPat
    ,getType
    ,resultingType
    ,typeFromAlternative
    )
    where

import Syntax
import ParserMonad(ParserM, returnOk, returnError)

import Data.List(nub)

import Text.PrettyPrint.Leijen(pretty)

-- checkPat checks that no variable is used twice in a pattern 
--          (aka it is a linear pattern)
-- TODO is this check really needed?
checkPat :: Pattern -> ParserM Pattern
checkPat pat =
    let
        varList = vars pat
    in
      if (length varList) == (length $ nub varList) then
          returnOk pat
      else
          returnError $ "Duplicated variable in pattern: " ++ (show $ pretty pat)
    where
      vars :: Pattern -> [Name]
      vars (VarPat n) = [n]
      vars (ConPat n ps) = ps
      vars (LitPat _) = []
      vars (ListPat ps) = ps
      vars (HeadTailPat n1 n2) = [n1, n2]
      vars (TuplePat ps) = ps
      vars (WildcardPat) = []

-- returns the type resuling for the application of FuncType
resultingType :: Type -> Maybe Type
resultingType (FuncType _ t) = Just t
resultingType _ = Nothing

getType :: Expr -> Type
getType (VarExp _ t) = t
getType (ConExp _ t) = t
getType (LitExp _ t) = t
getType (InfixOpExp _ t) = t
getType (FExp _ _ t) = t
getType (MinusExp _ t) = t
getType (LambdaExp _ _ t) = t
getType (LetExp _ _ t) = t
getType (IfExp _ _ _ t) = t
getType (CaseExp _ _ t) = t
getType (ParensExp _ t) = t
getType (TupleExp _ t) = t
getType (ListExp _ t) = t

typeFromAlternative :: Alternative -> Type
typeFromAlternative (Alternative _ e) = getType e

-- addType adds type information to an expression
addType :: Expr -> Type -> Expr
addType (VarExp n _) t = VarExp n t
addType (ConExp n _) t = ConExp n t
addType (LitExp v _) t = LitExp v t
addType (InfixOpExp e _) t = InfixOpExp e t
addType (FExp e e' _) t = FExp e e' t
addType (MinusExp e _) t = MinusExp e t
addType (LambdaExp p e _) t = LambdaExp p e t
addType (LetExp d e _) t = LetExp d e t
addType (IfExp e1  e2 e3 _) t = IfExp e1 e2 e3 t
addType (CaseExp e a _) t = CaseExp e a t
addType (ParensExp e _) t = ParensExp e t
addType (TupleExp e _) t = TupleExp e t
addType (ListExp e _) t = ListExp e t

-- litToExp creates an Expresion from a literal (with the right type)
litToExp :: LiteralValue -> Expr
litToExp val@(LiteralInt _) = LitExp val (ConType "Int")
litToExp val@(LiteralFloat _) = LitExp val (ConType "Float")
litToExp val@(LiteralString _) = LitExp val (ConType "String") -- TODO should this be [Char]??
litToExp val@(LiteralChar _) = LitExp val (ConType "Char")

-- assembleInfixOperator builds an infix operator structure (for fixity adaptation later)
assembleInfixOperator :: Expr -> Operator -> Expr -> Expr
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
