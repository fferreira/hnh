module EvalEnv
    (
     Env
    ,buildEvalEnv
    ,lookupEvalEnv
    )
    where

import Syntax
--import Value 

type Env = (Pattern, Exp)

buildEvalEnv :: Monad m => [Declaration] -> m [Env]
buildEvalEnv decls = mapM declToValue (filter isPatBind decls)
    where
      declToValue (PatBindDcl pat e) = return (pat, e)
      isPatBind (PatBindDcl _ _) = True
      isPatBind _ = False

lookupEvalEnv :: Monad m => Name -> [Env] -> m Env
lookupEvalEnv n ((p, e):envs) =
    if n `match` p then return (p, e) else lookupEvalEnv n envs
lookupEvalEnv n [] = fail $ "lookupEvalEnv: name " ++ n ++ " not found" 
 

match :: Name -> Pattern -> Bool
match n (VarPat n1 _) = n == n1
match n (ConPat n1 ns _) = (n == n1) || foldr (||) False (map (\n2 -> n == n2) ns)
match n (HeadTailPat n1 n2 _) = (n == n1) || (n == n2)
match n (TuplePat ns _) = foldr (||) False (map (\n2 -> n == n2) ns)
match n (WildcardPat _) = False