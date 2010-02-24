module BuiltIn
    (
     EnvType
    ,listType
    ,env0
    )
    where

import Syntax
import Value

type EnvType = (Name, Type)


listType = ConType "List" [VarType "a"] -- The defaultType of list 

env0 :: [EnvType] -- the initial environment, containing all the builin functions
env0 = 
    [("+", FuncType (ConType "Int" []) (FuncType (ConType "Int" []) (ConType "Int" [])))
    ,("-", FuncType (ConType "Int" []) (FuncType (ConType "Int" []) (ConType "Int" [])))
{-    ,("*", FuncType (ConType "Int" []) (FuncType (ConType "Int" []) (ConType "Int" [])))
    ,("/", FuncType (ConType "Int" []) (FuncType (ConType "Int" []) (ConType "Int" [])))
    ,("^", FuncType (ConType "Int" []) (FuncType (ConType "Int" []) (ConType "Int" []))) -}
    ,("~", FuncType (ConType "Int" []) (ConType "Int" []))

    ,("==", FuncType (ConType "Int" []) (FuncType (ConType "Int" []) (ConType "Bool" [])))
    ,( ">", FuncType (ConType "Int" []) (FuncType (ConType "Int" []) (ConType "Bool" [])))
    ,( "<", FuncType (ConType "Int" []) (FuncType (ConType "Int" []) (ConType "Bool" [])))
{-
    ,("+.", FuncType (ConType "Float" []) (FuncType (ConType "Float" []) (ConType "Float" [])))
    ,("-.", FuncType (ConType "Float" []) (FuncType (ConType "Float" []) (ConType "Float" [])))
    ,("*.", FuncType (ConType "Float" []) (FuncType (ConType "Float" []) (ConType "Float" [])))
    ,("/.", FuncType (ConType "Float" []) (FuncType (ConType "Float" []) (ConType "Float" [])))
    ,("^.", FuncType (ConType "Float" []) (FuncType (ConType "Float" []) (ConType "Float" [])))

    ,("==.", FuncType (ConType "Float" []) (FuncType (ConType "Float" []) (ConType "Bool" [])))
    ,( ">.", FuncType (ConType "Float" []) (FuncType (ConType "Float" []) (ConType "Bool" [])))
    ,( "<.", FuncType (ConType "Float" []) (FuncType (ConType "Float" []) (ConType "Bool" [])))
-}
    ,("True", ConType "Bool" [])
    ,("False", ConType "Bool" [])
    ]