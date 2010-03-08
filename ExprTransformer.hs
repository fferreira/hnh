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
import TransformMonad(TransformM)
import TransformUtils(transformTree, Transformer(..), defTrans)

type FixityDesc = (Operator, Precedence, Associativity)

correctPrecedence:: Program -> TransformM Program
correctPrecedence prog  = transformTree
                          "correctPrecedence: Non Associative operator used associatively"
                          defTrans {tExp  = adaptExpr }
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

-- Apply the transformation recursively to the right sided tree
transform ::[FixityDesc] -> OpExp -> Maybe OpExp
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

lst :: [FixityDesc] -> OpExp -> OpExp
lst tbl t@(Op p t1 (Op q t2 t3)) =
    if compPrecedence tbl p q then Op q (Op p t1 t2) t3 else t
lst _ t = t

-- toPrefix converts all the OpExp to FExp calls (prefix syntax)
toPrefix :: Program -> TransformM Program
toPrefix = transformTree
           "toPrefix: Unexpected Error (This should not fail)"
           defTrans { tExp = adaptExpr } -- (Transformer adaptExpr idM idM)
    where

      adaptExpr (InfixOpExp e _) = Just $ prefixer e
      adaptExpr (MinusExp e _) = 
          do
            e' <- adaptExpr e
            return (FExp 
                    (VarExp "~" UnknownType) 
                    e'
                    UnknownType)
      adaptExpr (MinusFloatExp e _) =
          do
            e' <- adaptExpr e
            return (FExp 
                    (VarExp "~." UnknownType) 
                    e'
                    UnknownType)
      adaptExpr e = Just e

      prefixer :: OpExp -> Exp
      prefixer (LeafExp e) = e
      prefixer (Op op e1 e2) = FExp 
                               (FExp (VarExp op UnknownType)
                                     (prefixer e1) 
                                     UnknownType)
                               (prefixer e2) 
                               UnknownType