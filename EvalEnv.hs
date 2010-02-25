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


import Data.List(intersperse)
import Text.PrettyPrint.Leijen -- requires wl-pprint installed (available in cabal)


type Env = (Pattern, Value)


env0 :: [Env]
env0 = [
  --- Integer Operations ---
 (VarPat "+" UnknownType, 
         Closure 
         [VarPat "a" (ConType "Int" []), VarPat "b" (ConType "Int" [])] [] (CFun add)),
 (VarPat "-" UnknownType, 
         Closure 
         [VarPat "a" (ConType "Int" []), VarPat "b" (ConType "Int" [])] [] (CFun sub)),
 (VarPat "*" UnknownType,
         Closure 
         [VarPat "a" (ConType "Int" []), VarPat "b" (ConType "Int" [])] [] (CFun mul)),
 (VarPat "/" UnknownType, 
         Closure 
         [VarPat "a" (ConType "Int" []), VarPat "b" (ConType "Int" [])] [] (CFun idiv)),
 (VarPat "~" UnknownType, 
         Closure 
         [VarPat "a" (ConType "Int" [])] [] (CFun neg)),

 (VarPat "==" UnknownType, 
         Closure 
         [VarPat "a" (ConType "Int" []), VarPat "b" (ConType "Int" [])] [] (CFun eq)),
 
 (VarPat ">" UnknownType, 
         Closure 
         [VarPat "a" (ConType "Int" []), VarPat "b" (ConType "Int" [])] [] (CFun gt)),

  --- Float Operations ---
 (VarPat "+." UnknownType, 
         Closure 
         [VarPat "a" (ConType "Float" []), VarPat "b" (ConType "Float" [])] [] (CFun fadd)),
 (VarPat "-." UnknownType, 
         Closure 
         [VarPat "a" (ConType "Float" []), VarPat "b" (ConType "Float" [])] [] (CFun fsub)),
 (VarPat "*." UnknownType,
         Closure 
         [VarPat "a" (ConType "Float" []), VarPat "b" (ConType "Float" [])] [] (CFun fmul)),
 (VarPat "/." UnknownType, 
         Closure 
         [VarPat "a" (ConType "Float" []), VarPat "b" (ConType "Float" [])] [] (CFun fdiv)),
 (VarPat "~." UnknownType, 
         Closure 
         [VarPat "a" (ConType "Int" [])] [] (CFun fneg)),

 (VarPat "==." UnknownType, 
         Closure 
         [VarPat "a" (ConType "Float" []), VarPat "b" (ConType "Float" [])] [] (CFun feq)),
 
 (VarPat ">." UnknownType, 
         Closure 
         [VarPat "a" (ConType "Float" []), VarPat "b" (ConType "Float" [])] [] (CFun fgt)),
 
  --- Support for lists ---
 (VarPat "Nil" UnknownType,
         ConVal "Nil" []),
 (VarPat "Cons" UnknownType,
         Closure
         [VarPat "p1" UnknownType, VarPat "p2" UnknownType] [] (CFun (buildData 2 "Cons"))),
  -- Suport for boolean type ---
 (VarPat "True" UnknownType,
         ConVal "True" []),
 (VarPat "False" UnknownType,
         ConVal "False" [])
 
 ]

lookupEvalEnv :: Monad m => Name -> [Env] -> m Env
lookupEvalEnv n ((p, e):envs) =
    if n `match` p then return (p, e) else lookupEvalEnv n envs
lookupEvalEnv n [] = fail $ "lookupEvalEnv: name " ++ n ++ " not found" 
 

match :: Name -> Pattern -> Bool
match n (VarPat n1 _) = n == n1
match n (ConPat n1 ns _) = (n == n1) || foldr (||) False (map (\n2 -> n == n2) ns)
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
    show v@(ConVal "Cons" _) = "["++concat (intersperse ", " (map show (consToList v)))++"]"
    show (ConVal n vs) = n ++ " " ++show vs
    show (TupleVal vs) = "#"++ show (map show vs)
    show (Closure pats env _) = "Closure"

instance Pretty Value where
    pretty v = pretty $ show v --TODO improve this

consToList :: Value -> [Value]
consToList (ConVal "Cons" [v1, v2]) = v1:consToList v2
consToList (ConVal "Nil" _) = []
consToList _ = [] 

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
      Just v -> (IntVal v)
      Nothing -> error "+ is of type Int -> Int -> Int"

sub :: IntrinsicFun
sub env =
    case( do
            (_, (IntVal a)) <- lookupEvalEnv "a" env
            (_, (IntVal b)) <- lookupEvalEnv "b" env
            return $ a - b) of
      Just v -> (IntVal v)
      Nothing -> error "- is of type Int -> Int -> Int"


mul :: IntrinsicFun
mul env =
    case( do
            (_, (IntVal a)) <- lookupEvalEnv "a" env
            (_, (IntVal b)) <- lookupEvalEnv "b" env
            return $ a * b) of
      Just v -> (IntVal v)
      Nothing -> error "* is of type Int -> Int -> Int"

idiv :: IntrinsicFun
idiv env =
    case( do
            (_, (IntVal a)) <- lookupEvalEnv "a" env
            (_, (IntVal b)) <- lookupEvalEnv "b" env
            return $ a `div` b) of
      Just v -> (IntVal v)
      Nothing -> error "/ is of type Int -> Int -> Int"

neg :: IntrinsicFun
neg env =
    case( do
            (_, (IntVal a)) <- lookupEvalEnv "a" env
            return $ -a) of
      Just v -> (IntVal v)
      Nothing -> error "- is of type Int -> Int -> Int"

eq :: IntrinsicFun
eq env =
    case( do
            (_, (IntVal a)) <- lookupEvalEnv "a" env
            (_, (IntVal b)) <- lookupEvalEnv "b" env
            return $ a == b) of
      Just v -> if v then (ConVal "True" []) else (ConVal "False" [])
      Nothing -> error "== is of type Int -> Int -> Bool"

gt :: IntrinsicFun
gt env =
    case( do
            (_, (IntVal a)) <- lookupEvalEnv "a" env
            (_, (IntVal b)) <- lookupEvalEnv "b" env
            return $ a > b) of
      Just v -> if v then (ConVal "True" []) else (ConVal "False" [])
      Nothing -> error "> is of type Int -> Int -> Bool"


fadd :: IntrinsicFun
fadd env =
    case( do
            (_, (FloatVal a)) <- lookupEvalEnv "a" env
            (_, (FloatVal b)) <- lookupEvalEnv "b" env
            return $ a + b) of
      Just v -> (FloatVal v)
      Nothing -> error "+. is of type Int -> Int -> Int"

fsub :: IntrinsicFun
fsub env =
    case( do
            (_, (FloatVal a)) <- lookupEvalEnv "a" env
            (_, (FloatVal b)) <- lookupEvalEnv "b" env
            return $ a - b) of
      Just v -> (FloatVal v)
      Nothing -> error "-. is of type Int -> Int -> Int"


fmul :: IntrinsicFun
fmul env =
    case( do
            (_, (FloatVal a)) <- lookupEvalEnv "a" env
            (_, (FloatVal b)) <- lookupEvalEnv "b" env
            return $ a * b) of
      Just v -> (FloatVal v)
      Nothing -> error "*. is of type Int -> Int -> Int"

fdiv :: IntrinsicFun
fdiv env =
    case( do
            (_, (FloatVal a)) <- lookupEvalEnv "a" env
            (_, (FloatVal b)) <- lookupEvalEnv "b" env
            return $ a / b) of
      Just v -> (FloatVal v)
      Nothing -> error "/. is of type Int -> Int -> Int"

fneg :: IntrinsicFun
fneg env =
    case( do
            (_, (FloatVal a)) <- lookupEvalEnv "a" env
            return $ -a) of
      Just v -> (FloatVal v)
      Nothing -> error "- is of type Int -> Int -> Int"

feq :: IntrinsicFun
feq env =
    case( do
            (_, (FloatVal a)) <- lookupEvalEnv "a" env
            (_, (FloatVal b)) <- lookupEvalEnv "b" env
            return $ a == b) of
      Just v -> if v then (ConVal "True" []) else (ConVal "False" [])
      Nothing -> error "==. is of type Int -> Int -> Bool"

fgt :: IntrinsicFun
fgt env =
    case( do
            (_, (FloatVal a)) <- lookupEvalEnv "a" env
            (_, (FloatVal b)) <- lookupEvalEnv "b" env
            return $ a > b) of
      Just v -> if v then (ConVal "True" []) else (ConVal "False" [])
      Nothing -> error ">. is of type Int -> Int -> Bool"

