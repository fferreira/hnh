module EvalEnv
    (
     Env
    ,lookupEvalEnv
    ,Value(..)
    )
    where

import Syntax



import Text.PrettyPrint.Leijen -- requires wl-pprint installed (available in cabal)

type Env = (Pattern, Value)



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

      
data Value = LitVal LiteralValue
           | ConVal Name -- a constructor, such as "True"
           | Closure [Pattern] [Env] Exp
             deriving (Show, Eq)

instance Pretty Value where
    pretty v = pretty $ show v --TODO improve this