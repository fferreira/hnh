module ExprTransformer
    (
     correctPrecedence
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

type FixityDesc = (Operator, Precedence, Associativity)

-- TODO add error handling
correctPrecedence :: Program -> TransformM Program
correctPrecedence prog@(Program decls) = transformOk $ Program (map adaptDeclaration decls)
    where
      adaptDeclaration (FunBindDcl n pats rhs) = FunBindDcl n pats (adaptRhs rhs)
      adaptDeclaration (PatBindDcl pat rhs) = PatBindDcl pat (adaptRhs rhs)
      adaptDeclaration d = d

      adaptRhs (UnGuardedRhs e) = UnGuardedRhs (adaptExpr e)
      adaptRhs (GuardedRhs guards) = GuardedRhs (map adaptGuard guards)

      adaptGuard (Guard e1 e2) = Guard (adaptExpr e1) (adaptExpr e2)

      adaptExpr (InfixOpExp e t) = InfixOpExp (transform precedences e) t
      adaptExpr e = e

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

transform ::[FixityDesc] -> OpExpr -> OpExpr
transform tbl t@(LeafExp _) = t
transform tbl t@(Op _ _ (LeafExp _)) = t 
transform tbl t@(Op o left t'@(Op _ _ _)) = lst tbl (Op o left (transform  tbl (lst tbl t')))

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

{-  Testing data

tbl  :: [FixityDesc]
tbl =
    [
     ("+" , 6, LeftAssoc)
    ,("-" , 6, LeftAssoc)
    ,("*" , 7, LeftAssoc)
    ,("/" , 7, LeftAssoc)
    ,("^" , 8, RightAssoc)
    ,("==", 4, NonAssoc)
    ,("<", 4, NonAssoc)
    ,(">", 4, NonAssoc)
    ];

  
testTree = (Op "^" (LeafExp (VarExp "a" UnknownType)) 
                   (Op "*" (LeafExp (VarExp "b" UnknownType)) 
                           (Op "^" (LeafExp (VarExp "c" UnknownType))
                                   (LeafExp (VarExp "d" UnknownType)))))
-} 
