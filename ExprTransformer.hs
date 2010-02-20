module ExprTransformer
    (
     correctPrecedence
    ,toPrefix
    )
    where
{-
This module will transform the AST 
to reflect the right associativity 
and precedence of operators.

The algorithm is from: 
Wilf Lalonde and Jum Des Rivieres, "Handling Operator Precedence in Artihmetic Expressions with Tree Transformations", 1981
-}

import Syntax
import TransformMonad
import TransformUtils(transformExpressions)

type FixityDesc = (Operator, Precedence, Associativity)


correctPrecedence:: Program -> TransformM Program
correctPrecedence prog = transformExpressions
                         "correctPrecedence: Non Associative operator used associatively"
                         adaptExpr
                         prog
    where
      adaptExpr (InfixOpExp e t) = 
          do
            e' <- transform precedences e
            return $ InfixOpExp e' t
      adaptExpr e = Just e

      precedences = buildPrecedenceTable prog
      

buildPrecedenceTable :: Program -> [FixityDesc]
buildPrecedenceTable (Program declarations) =
    let
        fixity = filter isFixity declarations
        isFixity = \d -> case d of
                           FixityDcl _ _ _ -> True
                           otherwise -> False
        explode (opList, p, a) = map (\x -> (x, p, a)) opList
    in
      concatMap (\(FixityDcl a p o) -> explode(o, p, a)) fixity

lookupPrecedence :: [FixityDesc] -> Operator -> FixityDesc
lookupPrecedence tbl op = case filter (\(o, p, a) -> op == o) tbl of
                            [] -> (op, 9, LeftAssoc) -- default fixity
                            l  -> head l             -- return the first

transform ::[FixityDesc] -> OpExpr -> Maybe OpExpr
transform tbl t@(LeafExp _) = Just t
transform tbl t@(Op _ _ (LeafExp _)) = Just t 
transform tbl t@(Op o left t'@(Op _ _ _)) =
    do
      transformed <- transform  tbl (lst tbl t')
      return $ lst tbl (Op o left transformed)

-- precedence operator, returns whether o1 has a tighter precedence, or left associativity
compPrecedence :: [FixityDesc] -> Operator -> Operator -> Bool
compPrecedence table o1 o2 =
    let
        (_, o1p, o1a) = lookupPrecedence table o1
        (_, o2p, _) = lookupPrecedence table o2
        -- if o1 binds tighter than o2, or for equal precedence, if o1 is left associative
        res = (o1p > o2p) || ((o1p == o2p) && (o1a == LeftAssoc)) 
    in
      (res || 
          (if (o1p == o2p) && (o1a == NonAssoc) then
              error "Non associative operator used without ()" --TODO improve error handling
          else
              False))

-- Left Subordinate Transform 

lst :: [FixityDesc] -> OpExpr -> OpExpr
lst tbl t@(Op p t1 (Op q t2 t3)) =
    if compPrecedence tbl p q then Op q (Op p t1 t2) t3 else t
lst _ t = t

-- prefixer converts all the OpExpr to FExp calls (prefix syntax)
toPrefix :: Program -> TransformM Program
toPrefix prog = transformExpressions
                "toPrefix: Unexpected Error (This should not fail)"
                adaptExpr
                prog
    where

      adaptExpr (InfixOpExp e _) = Just $ prefixer e -- TODO add type?
      adaptExpr e = Just e

      prefixer :: OpExpr -> Expr
      prefixer (LeafExp e) = e
      prefixer (Op op e1 e2) = FExp 
                               (VarExp op UnknownType) 
                               (FExp (prefixer e1) (prefixer e2) UnknownType)
                               UnknownType