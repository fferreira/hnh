module BuiltIn
       (
         builtInDecls
       , resultId
       )
       where

import Syntax
import CPSRep

import Control.Monad.State(State, get, put, evalState)

type EnvType = (Name, Type)
type IdEnvType = (Identifier, Type)

resultId = (Id "end" 0)

-- the initial environment, containing all the builtin functions
env0 :: [EnvType] 
env0 = 
    [("+", FunType (PrimType "Int") (FunType (PrimType "Int") (PrimType "Int")))
    ,("-", FunType (PrimType "Int") (FunType (PrimType "Int") (PrimType "Int")))
    ,("*", FunType (PrimType "Int") (FunType (PrimType "Int") (PrimType "Int")))
    ,("/", FunType (PrimType "Int") (FunType (PrimType "Int") (PrimType "Int")))
    -- ,("^", FunType (PrimType "Int") (FunType (PrimType "Int") (PrimType "Int")))
    ,("~", FunType (PrimType "Int") (PrimType "Int"))

    ,("==", FunType (PrimType "Int") (FunType (PrimType "Int") (DataType "Bool" [])))
    ,( ">", FunType (PrimType "Int") (FunType (PrimType "Int") (DataType "Bool" [])))
    ,( "<", FunType (PrimType "Int") (FunType (PrimType "Int") (DataType "Bool" [])))
    
    ,("+.", FunType (PrimType "Float") (FunType (PrimType "Float") (PrimType "Float")))
    ,("-.", FunType (PrimType "Float") (FunType (PrimType "Float") (PrimType "Float")))
    ,("*.", FunType (PrimType "Float") (FunType (PrimType "Float") (PrimType "Float")))
    ,("/.", FunType (PrimType "Float") (FunType (PrimType "Float") (PrimType "Float")))
    -- ,("^.", FunType (PrimType "Float") (FunType (PrimType "Float") (PrimType "Float")))

    ,("==.", FunType (PrimType "Float") (FunType (PrimType "Float") (DataType "Bool" [])))
    ,( ">.", FunType (PrimType "Float") (FunType (PrimType "Float") (DataType "Bool" [])))
    ,( "<.", FunType (PrimType "Float") (FunType (PrimType "Float") (DataType "Bool" [])))
    ]
    
builtInDecls :: [Declaration]                
builtInDecls = [DataDcl 
                (DataType "List" [VarType "a"]) 
                [ConDcl "Cons" [VarType "a"
                               ,DataType "List" [VarType "a"]
                               ]
                ,ConDcl "Nil" []
                ]
               ,DataDcl
                (DataType "Bool" []) [ConDcl "True" []
                                     ,ConDcl "False" []
                                     ]
               ]
               ++ builtInFuns env0
                
builtInFuns :: [EnvType] -> [Declaration]
builtInFuns builtIns = map toDecl builtIns 
  where
    toDecl :: EnvType -> Declaration
    toDecl (n, t) = PatBindDcl 
                    (VarPat n t) 
                    (LambdaExp pats (Prim n params (getLast t)) UnknownType)
      where
        pats = map (\n -> (VarPat n UnknownType)) params
        params = map (\c->[c]) (take (countParams t) varNames)
    
    varNames = "abcdefghijkalmnopqrstuvwxyz"
    
    getLast (FunType _ t2) = getLast t2
    getLast t = t
    

countParams :: Type -> Int
countParams t = countParams' t 0
  where
    countParams' :: Type -> Int -> Int
    countParams' (FunType t1 t2) n = countParams' t1 (countParams' t2 (n+1))
    countParams' _ n = n