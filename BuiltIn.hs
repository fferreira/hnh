module BuiltIn
       (
         EnvType
       , env0
       , builtInDataTs
       )
       where

import Syntax
import TypeUtils(DataType)

type EnvType = (Name, Type)


env0 :: [EnvType] -- the initial environment, containing all the builin functions
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
    ,("True", DataType "Bool" [])
    ,("False", DataType "Bool" [])
     
    ,("Nil", DataType "List" [VarType "a"])
    ,("Cons", FunType (VarType "a") (FunType 
                                      (DataType "List" [VarType "a"])
                                      (DataType "List" [VarType "a"])))
                                    
    ]
    
    
builtInDataTs :: [DataType]    
builtInDataTs = [(DataType "List" [VarType "a"], [ConDcl "Cons" [VarType "a"
                                                                ,DataType "List" [VarType "a"]
                                                                ]
                                                 ,ConDcl "Nil" []
                                                 ])
                ,(DataType "Bool" [], [ConDcl "True" []
                                      ,ConDcl "False" []
                                      ])
                ]