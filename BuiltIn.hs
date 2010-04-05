module BuiltIn
       (
         EnvType
       , env0
       , idEnv0
       , builtInDecls
       , builtInContinuation
       )
       where

import Syntax
import CPSRep

type EnvType = (Name, Type)
type IdEnvType = (Identifier, Type)


env0 :: [EnvType] -- the initial environment, containing all the builtin functions
env0 = 
    [("+", FunType (PrimType "Int") (FunType (PrimType "Int") (PrimType "Int")))
    ,("-", FunType (PrimType "Int") (FunType (PrimType "Int") (PrimType "Int")))
{-    ,("*", FunType (PrimType "Int") (FunType (PrimType "Int") (PrimType "Int")))
    ,("/", FunType (PrimType "Int") (FunType (PrimType "Int") (PrimType "Int")))
    ,("^", FunType (PrimType "Int") (FunType (PrimType "Int") (PrimType "Int"))) -}
    ,("~", FunType (PrimType "Int") (PrimType "Int"))

    ,("==", FunType (PrimType "Int") (FunType (PrimType "Int") (DataType "Bool" [])))
    ,( ">", FunType (PrimType "Int") (FunType (PrimType "Int") (DataType "Bool" [])))
    ,( "<", FunType (PrimType "Int") (FunType (PrimType "Int") (DataType "Bool" [])))
{-
    ,("+.", FunType (PrimType "Float") (FunType (PrimType "Float") (PrimType "Float")))
    ,("-.", FunType (PrimType "Float") (FunType (PrimType "Float") (PrimType "Float")))
    ,("*.", FunType (PrimType "Float") (FunType (PrimType "Float") (PrimType "Float")))
    ,("/.", FunType (PrimType "Float") (FunType (PrimType "Float") (PrimType "Float")))
    ,("^.", FunType (PrimType "Float") (FunType (PrimType "Float") (PrimType "Float")))

    ,("==.", FunType (PrimType "Float") (FunType (PrimType "Float") (DataType "Bool" [])))
    ,( ">.", FunType (PrimType "Float") (FunType (PrimType "Float") (DataType "Bool" [])))
    ,( "<.", FunType (PrimType "Float") (FunType (PrimType "Float") (DataType "Bool" [])))
-}
    ]
    
idEnv0 :: [IdEnvType]
idEnv0 = zip (map 
              (\(n,num) -> (Id n num)) 
              (zip 
               (fst (unzip env0)) 
               [0..(length env0 - 1)]))
         (snd (unzip env0))
    
builtInDecls :: [Declaration]                
builtInDecls = [DataDcl 
                (DataType "List" [VarType "a"]) [ConDcl "Cons" [VarType "a"
                                                               ,DataType "List" [VarType "a"]
                                                                       ]
                                                ,ConDcl "Nil" []
                                                ]
               ,DataDcl
                (DataType "Bool" []) [ConDcl "True" []
                                     ,ConDcl "False" []
                                     ]
               ]
                
builtInContinuation :: KExp -> KExp
builtInContinuation k = build idEnv0 
  where
    build [(id, t)] = PrimK id k t
    build ((id, t):env) = PrimK id (build env) t