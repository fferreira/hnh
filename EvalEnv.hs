module EvalEnv
    (
     Env
    ,buildEvalEnv
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

lookupEvalEnv :: Monad m => [Env] -> m Env
lookupEvalEnv env = fail "lev"
--    case lookup 