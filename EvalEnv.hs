module EvalEnv
    (
     Env
    ,env0
    ,lookupEvalEnv
    ,envForData
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
         [VarPat "a" (ConType "Int" []), VarPat "b" (ConType "Int" [])] [] (CFun mul)),
 (VarPat "Nil" UnknownType,
         ConVal "Nil" []),

 (VarPat "Cons" UnknownType,
         Closure
         [VarPat "p1" UnknownType, VarPat "p2" UnknownType] [] (CFun (buildData 2 "Cons")))

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
           | ConVal Name [Value]-- a constructor, such as "True"
           | TupleVal [Value]
           | Closure [Pattern] [Env] ClosureAction

data ClosureAction = CExp Exp |CFun IntrinsicFun

type IntrinsicFun = [Env] -> Value

instance Show Value where
    show (IntVal a) = show a
    show (FloatVal a) = show a
    show (StringVal a) = a
    show (CharVal a) = a
    show (ConVal n vs) = n ++ " " ++show vs
    show (TupleVal vs) = "#"++ show (map show vs)
    show (Closure pats env _) = "Closure"

instance Pretty Value where
    pretty v = pretty $ show v --TODO improve this

-- builds a closure to represent a data type constructor
envForData :: Name -> [ConstructorDeclaration] -> [Env]
envForData n ((ConDcl nCons types):cs) = ((VarPat nCons) UnknownType, 
                                          if length types == 0 
                                          then (ConVal nCons []) 
                                          else (Closure pats env (CFun(buildData (length types) nCons))))
                                         : envForData n cs
    where
      pats = map (\i ->(VarPat ("p"++show i) UnknownType)) [1..(length types)]
      env = [(VarPat "val" UnknownType, ConVal n []), 
             (VarPat "numParams" UnknownType, IntVal (length types))]
envForData n [] = []

-- builds a value of a data type constructor
buildData:: Int -> Name-> IntrinsicFun
buildData l n env = 
    case (do
           pats <- return $ map (\i ->(VarPat ("p"++show i) UnknownType)) [1..l]
           pvs <- mapM (\(VarPat n _) -> lookupEvalEnv n env) pats
           return $ ConVal n (snd (unzip pvs))) of
      Just v -> v
      Nothing -> error "Unable to build data value"

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