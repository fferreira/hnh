module EvalEnv
    (
     Env
    ,env0
    ,lookupEvalEnv
    ,Value(..)
    ,ClosureAction(..)
    )
    where

import Syntax



import Text.PrettyPrint.Leijen -- requires wl-pprint installed (available in cabal)

type Env = (Pattern, Value)


env0 :: [Env]
env0 = [
 (VarPat "+" UnknownType, 
         Closure 
         [VarPat "a" (ConType "Int" []), VarPat "b" (ConType "Int" [])] [] (CFun add)),
 (VarPat "*" UnknownType, 
         Closure 
         [VarPat "a" (ConType "Int" []), VarPat "b" (ConType "Int" [])] [] (CFun mul))
 ]

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

data Value = IntVal Int
           | FloatVal Double
           | StringVal String
           | CharVal String
           | ConVal Name -- a constructor, such as "True"
           | TupleVal [Value]
           | Closure [Pattern] [Env] ClosureAction

data ClosureAction = CExp Exp |CFun IntrinsicFun

type IntrinsicFun = [Env] -> Value

instance Show Value where
    show (IntVal a) = show a
    show (FloatVal a) = show a
    show (StringVal a) = a
    show (CharVal a) = a
    show (ConVal n) = n
    show (TupleVal vs) = "#"++ show (map show vs)
    show (Closure pats env _) = "Closure"

instance Pretty Value where
    pretty v = pretty $ show v --TODO improve this

add :: IntrinsicFun
add env =
    case( do
            (_, (IntVal a)) <- lookupEvalEnv "a" env
            (_, (IntVal b)) <- lookupEvalEnv "b" env
            return $ a + b) of
      Just sum -> (IntVal sum)
      Nothing -> error "+ is of type Int -> Int -> Int"

mul :: IntrinsicFun
mul env =
    case( do
            (_, (IntVal a)) <- lookupEvalEnv "a" env
            (_, (IntVal b)) <- lookupEvalEnv "b" env
            return $ a * b) of
      Just mul -> (IntVal mul)
      Nothing -> error "+ is of type Int -> Int -> Int"