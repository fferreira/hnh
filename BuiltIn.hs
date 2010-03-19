module BuiltIn
    (
     EnvType
    ,listType
    ,env0
    )
    where

import Syntax

type EnvType = (Name, Type)


listType = DataType "List" [VarType "a"] -- The defaultType of list 

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