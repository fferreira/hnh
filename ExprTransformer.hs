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
    ,literalStringElimination
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

import Control.Monad(filterM)

type FixityDesc = (Operator, Precedence, Associativity)

correctPrecedence:: Program -> TransformM Program
correctPrecedence prog  = transformTree
                          "correctPrecedence"
                          defTrans {tExp  = adaptExpr, tDecls = adaptDecls }
                          prog
    where
      adaptExpr (InfixOpExp e t) = 
          do
            precedences <- buildPrecedenceTable prog
            e' <- transform precedences e
            return $ InfixOpExp e' t
      adaptExpr e = return e
      
      adaptDecls decls = return $ filter (not . isFixity) decls
      isFixity (FixityDcl _ _ _) = True
      isFixity _ = False

buildPrecedenceTable :: Monad m => Program -> m [FixityDesc]
buildPrecedenceTable (Program decls) =
    do
      fixity <- filterM isFixity decls
      precedences <- mapM (\(FixityDcl a p o) -> explode(o, p, a)) fixity
      return $ concat precedences
    where
      isFixity (FixityDcl _ _ _) = return True
      isFixity _ = return False

      explode (opList, p, a) = mapM (\x -> return (x, p, a)) opList

lookupPrecedence :: [FixityDesc] -> Operator -> FixityDesc
lookupPrecedence tbl op = case filter (\(o, p, a) -> op == o) tbl of
                            [] -> (op, 9, LeftAssoc) -- default fixity
                            l  -> head l             -- return the first

-- Apply the transformation recursively to the right sided tree
transform ::Monad m => [FixityDesc] -> OpExp -> m OpExp
transform tbl t@(LeafExp _) = return t
transform tbl t@(Op _ _ (LeafExp _)) = return t 
transform tbl t@(Op o left right@(Op _ _ _)) =
    do
      right' <- lst tbl right
      transformed <- transform  tbl right'
      lst tbl (Op o left transformed)

-- precedence operator, returns whether o1 has a tighter precedence, or left associativity
compPrecedence :: Monad m => [FixityDesc] -> Operator -> Operator -> m Bool
compPrecedence table o1 o2 =
    let
        (_, o1p, o1a) = lookupPrecedence table o1
        (_, o2p, _) = lookupPrecedence table o2
        -- if o1 binds tighter than o2, or for equal precedence, if o1 is left associative
        res = (o1p > o2p) || ((o1p == o2p) && (o1a == LeftAssoc)) 
    in
      if res then return True
         else 
             if (o1p == o2p) && (o1a == NonAssoc) then
                 fail "Non associative operator used without ()"
             else
                 return False

-- Left Subordinate Transform 

lst :: Monad m => [FixityDesc] -> OpExp -> m OpExp
lst tbl t@(Op p t1 (Op q t2 t3)) =
    do
      should <- compPrecedence tbl p q
      if should then return $ Op q (Op p t1 t2) t3 else return t
lst _ t = return t

-- toPrefix converts all the OpExp to FExp calls (prefix syntax)
toPrefix :: Program -> TransformM Program
toPrefix = transformTree
           "toPrefix"
           defTrans { tExp = adaptExpr }
    where

      adaptExpr (InfixOpExp e _) = return $ prefixer e
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
      adaptExpr e = return e

      prefixer :: OpExp -> Exp
      prefixer (LeafExp e) = e
      prefixer (Op op e1 e2) = FExp 
                               (FExp (VarExp op UnknownType)
                                     (prefixer e1) 
                                     UnknownType)
                               (prefixer e2) 
                               UnknownType

-- literalStringElimination converts all LiteralExp String to lists of chars
literalStringElimination :: Program -> TransformM Program
literalStringElimination = transformTree
                           "literalStringElimination"
                           defTrans { tExp = adaptExpr }
                             where
                               adaptExpr (LitExp (LiteralString s) _) = 
                                 return $ strToConExp s
                               adaptExpr e = return e
                               
                               strToConExp s = ListExp (map (\c -> (LitExp 
                                                                    (LiteralChar [c]) 
                                                                    UnknownType)) s)
                                               UnknownType