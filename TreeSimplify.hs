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

module TreeSimplify
    (
     funToLambda
    ,simplifyLambda
    ,simplifyPatterns
    )
    where

import Syntax
import TransformMonad (TransformM)
import TransformUtils(transformTree, Transformer(..), defTrans)

import Data.List(sort, group, groupBy, intersperse)

{-
  funToLambda: eliminates all the FunBindDcl transforming them to lambdas
-}
funToLambda:: Program -> TransformM Program
funToLambda prog  = transformTree
                    "funToLambda"
                    defTrans {tDecls  = declsToLambda }
                    prog

declsToLambda :: Monad m => [Declaration] -> m [Declaration]
declsToLambda decls = mergePatternsInCase decls

mergePatternsInCase :: Monad m => [Declaration] -> m [Declaration]
mergePatternsInCase decls =
    do
        groups <- return $ groupBy sameName decls 
        mapM convertGroup groups
      
convertGroup :: Monad m => [Declaration] -> m Declaration
convertGroup funs@((FunBindDcl _ _ _):_) =
    do
        name <- getFunName (head funs)
        patterns <- mapM getFunPattern funs
        rhss <- mapM getFunRhs funs
        validePatternLength patterns
        rhs <- generateLambdas (zip patterns rhss)
        return (PatBindDcl (VarPat name UnknownType) rhs)

convertGroup otherDecls = return $ head otherDecls

{- all functions should take the same number of parameters -}
validePatternLength :: Monad m => [[Pattern]] -> m Bool
validePatternLength pats = 
    do
      if (length . group . sort) (map (\p -> length p) pats) == 1
        then
            return True
        else
            fail "not all functions take the same number of parameters"


generateLambdas :: Monad m => [([Pattern], Exp)] -> m Exp
generateLambdas [(p, e)] = return (LambdaExp p e UnknownType)
generateLambdas prs = return (LambdaExp pats
                              (CaseExp exps alts UnknownType) UnknownType)
    where
      alts = map (\(p, e) -> (Alternative p e)) prs
      pats = map (\s -> (VarPat s UnknownType)) names
      exps = map (\s -> (VarExp s UnknownType)) names
      names = map (\i -> "p#"++ show i) [1..((length.fst.head) prs)] 

sameName (FunBindDcl n1 _ _) (FunBindDcl n2 _ _) = n1 == n2
sameName _ _ = False

getFunName (FunBindDcl n _ _) = return n
getFunName _ = fail "Unexpected getFunName called with something other than a function"

getFunPattern (FunBindDcl _ p _) = return p
getFunPattern _ = fail "getFunPattern: called with something other than a function"

getFunRhs (FunBindDcl _ _ r) = return r
getFunRhs _ = fail "getFunRhs: called with something ohter than a function"

{-
  simplifyLambda: pushes all complex pattern matching to case expressions
-}
simplifyLambda:: Program -> TransformM Program
simplifyLambda prog  = transformTree
                       "simplifyLambda"
                       defTrans {tExp  = transformLambda }
                       prog


transformLambda :: Monad m => Exp -> m Exp
transformLambda (LambdaExp pats e t) = return $ LambdaExp (reverse pats') e' t
    where
      newPats = map (\i ->(VarPat ("p#"++show i) UnknownType )) [1..(length pats)]

      merge :: Pattern -> Pattern -> ([Pattern], Exp) -> ([Pattern], Exp)
      merge p1 p2@(VarPat n2 _) (p, e) = if isSimple p1 then (p1:p, e)
                                           else
                                             (p2:p, (CaseExp 
                                                     [(VarExp n2 UnknownType)] 
                                                     [Alternative [p1] e] UnknownType))

      isSimple (VarPat _ _) = True
      isSimple _ = False

      process :: [Pattern] -> [Pattern] -> ([Pattern], Exp) -> ([Pattern], Exp)
      process (op:ops) (np:nps) (pats,e) = merge op np (process ops nps (pats, e)) 
      process [] [] (pats, e) = (pats, e)

      (pats', e') = process (reverse pats) newPats ([], e)

transformLambda e = return e

{-
  simplifyPatterns will transfrom all pattern bindings into pattern bindings
  that only use var patterns
-}

simplifyPatterns :: Program -> TransformM Program
simplifyPatterns prog =  transformTree
                         "simplifyPattern"
                         defTrans {tDecls  =  transformPatBinds }
                         prog


transformPatBinds :: Monad m => [Declaration] -> m [Declaration]
transformPatBinds decls =
    do
      decls' <- mapM transformPatBind decls
      return $ concat decls'

transformPatBind :: Monad m => Declaration -> m [Declaration]
transformPatBind (PatBindDcl pat@(ConPat _ ns _) e) = 
    do
      valName <- return $ concat (intersperse "#" ns) ++ "#"
      ds <- mapM (buildDecl pat valName) ns
      return $ (PatBindDcl (VarPat valName UnknownType) e):ds

transformPatBind  (PatBindDcl pat@(TuplePat ns _) e) =
    do
      valName <- return $ concat (intersperse "#" ns) ++ "#"
      ds <- mapM (buildDecl pat valName) ns
      return $ (PatBindDcl (VarPat valName UnknownType) e):ds

transformPatBind p = return [p];

-- builds a declaration to extract a variable from the 'patterned' value
buildDecl :: Monad m => Pattern -> Name -> Name -> m Declaration
buildDecl pat valName name = return $ PatBindDcl (VarPat name UnknownType) 
                             (FExp (LambdaExp [pat] (VarExp name UnknownType)  UnknownType) 
                                       (VarExp valName UnknownType) UnknownType)