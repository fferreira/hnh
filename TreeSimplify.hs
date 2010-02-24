module TreeSimplify
    (
     funToLambda
    ,simplifyLambda
    )
    where

import Syntax
import TransformMonad (TransformM, transformOk)
import TransformUtils(transformTree, Transformer(..), defTrans)

import Data.List(sort, groupBy, nub)

{-
  funToLambda: eliminates all the FunBindDcl transforming them to lambdas
-}
funToLambda:: Program -> TransformM Program
funToLambda prog  = transformTree
                    "funToLambda: unable to generate lambdas"
                    defTrans {tDecls  = declsToLambda }
                    prog

declsToLambda :: [Declaration] -> Maybe [Declaration]
declsToLambda decls =
    let
        (funs, other) = partition isFun decls
        isFun (FunBindDcl _ _ _ _) = True
        isFun _ = False
    in
      do
        funs' <- mergePatternsInCase funs
        return $ other ++ funs'

mergePatternsInCase :: Monad m => [Declaration] -> m [Declaration]
mergePatternsInCase decls =
    do
        -- TODO do we have to sort the list? or the definitions should be contiguous?
        groups <- return $ groupBy sameName (sort decls) 
        mapM convertGroup groups
      
convertGroup :: Monad m => [Declaration] -> m Declaration
convertGroup funs =
    do
        name <- getFunName (head funs)
        patterns <- mapM getFunPattern funs
        rhss <- mapM getFunRhs funs
        validePatternLength patterns
        rhs <- generateLambdas (zip patterns rhss)
        return (PatBindDcl (VarPat name UnknownType) rhs)

{- if there are more than one function all patterns should have length 1 -}
validePatternLength :: Monad m => [[Pattern]] -> m Bool
validePatternLength pats = 
    do
      if (length pats == 1)||(foldr (&&) True (map (\p -> length p == 1) pats))
        then
            return True
        else
            fail "not all lengths of the same long!"

generateLambdas :: Monad m => [([Pattern], Exp)] -> m Exp
generateLambdas [(p, e)] = return (LambdaExp p e UnknownType)
generateLambdas prs = return (LambdaExp [(VarPat "x#" UnknownType)] 
                              (CaseExp (VarExp "x#" UnknownType) alts UnknownType) UnknownType)
    where
      alts = map (\(p:ps, e) -> (Alternative p e)) prs

sameName (FunBindDcl n1 _ _ _) (FunBindDcl n2 _ _ _) = n1 == n2
sameName _ _ = False

getFunName (FunBindDcl n _ _ _) = return n
getFunName _ = error "Unexpected getFunName called with something other than a function"

getFunPattern (FunBindDcl _ p _ _) = return p
getFunPattern _ = fail "getFunPattern: called with something other than a function"

getFunRhs (FunBindDcl _ _ r _) = return r
getFunRhs _ = fail "getFunRhs: called with something ohter than a function"

partition :: (a->Bool) -> [a] ->([a],[a])                   
partition f (x:xs) = (if f x then ([x], []) else ([], [x])) +-+ partition f xs
    where
      (+-+) (a, b) (c, d) = (a++c, b++d)
partition f [] = ([], [])                   

{-
  simplifyLambda: pushes all comples pattern matching to case expressions
-}
simplifyLambda:: Program -> TransformM Program
simplifyLambda prog  = transformTree
                       "simplifyLambda: unable to simplify lambdas"
                       defTrans {tExp  = transformLambda }
                       prog


transformLambda :: Monad m => Exp -> m Exp
transformLambda (LambdaExp pats e t) = return $ LambdaExp pats' e' t
    where
      newPats = map (\i ->(VarPat ("p#"++show i) UnknownType )) [1..(length pats)]

      merge :: Pattern -> Pattern -> ([Pattern], Exp) -> ([Pattern], Exp)
      merge p1 p2@(VarPat n2 _) (p, e) = if isSimple p1 then (p1:p, e)
                                           else
                                             (p2:p, (CaseExp 
                                                     (VarExp n2 UnknownType) 
                                                     [Alternative p1 e] UnknownType))

      isSimple (VarPat _ _) = True
      isSimple _ = False

      process :: [Pattern] -> [Pattern] -> ([Pattern], Exp) -> ([Pattern], Exp)
      process (op:ops) (np:nps) (pats,e) = merge op np (process ops nps (pats, e)) 
      process [] [] (pats, e) = (pats, e)

      (pats', e') = process (reverse pats) newPats ([], e)

transformLambda e = return e