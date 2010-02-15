module Types
    (
     addType
    ,litToExp
    )
    where

import Syntax

addType :: Expr -> Type -> Expr
addType (VarExp n _) t = VarExp n t
addType (ConExp n _) t = ConExp n t
addType (LitExp v _) t = LitExp v t
addType (InfixOpExp e o e' _) t = InfixOpExp e o e' t
addType (FExp e e' _) t = FExp e e' t
addType (MinusExp e _) t = MinusExp e t
addType (LambdaExp p e _) t = LambdaExp p e t
addType (LetExp d e _) t = LetExp d e t
addType (IfExp e1  e2 e3 _) t = IfExp e1 e2 e3 t
addType (CaseExp e a _) t = CaseExp e a t
addType (ParensExp e _) t = ParensExp e t
addType (TupleExp e _) t = TupleExp e t
addType (ListExp e _) t = ListExp e t
addType (LeftSectionExp e o _) t = LeftSectionExp e o t
addType (RightSectionExp o e _) t = RightSectionExp o e t
addType (ArithSeqExp e1 e2 e3 _) t = ArithSeqExp e1 e2 e3 t

litToExp :: LiteralValue -> Expr
litToExp val@(LiteralInt _) = LitExp val (ConsType "Int")
litToExp val@(LiteralFloat _) = LitExp val (ConsType "Float")
litToExp val@(LiteralString _) = LitExp val (ConsType "String") -- TODO should this be [Char]??
litToExp val@(LiteralChar _) = LitExp val (ConsType "Char")
