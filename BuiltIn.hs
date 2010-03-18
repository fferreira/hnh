module BuiltIn
    (
     EnvType
    ,listType
    ,env0
    )
    where

import Syntax

type EnvType = (Name, Type)


listType = ConType "List" [VarType "a"] -- The defaultType of list 

env0 :: [EnvType] -- the initial environment, containing all the builin functions
env0 = 
    [("+", FunType (ConType "Int" []) (FunType (ConType "Int" []) (ConType "Int" [])))
    ,("-", FunType (ConType "Int" []) (FunType (ConType "Int" []) (ConType "Int" [])))
{-    ,("*", FunType (ConType "Int" []) (FunType (ConType "Int" []) (ConType "Int" [])))
    ,("/", FunType (ConType "Int" []) (FunType (ConType "Int" []) (ConType "Int" [])))
    ,("^", FunType (ConType "Int" []) (FunType (ConType "Int" []) (ConType "Int" []))) -}
    ,("~", FunType (ConType "Int" []) (ConType "Int" []))

    ,("==", FunType (ConType "Int" []) (FunType (ConType "Int" []) (ConType "Bool" [])))
    ,( ">", FunType (ConType "Int" []) (FunType (ConType "Int" []) (ConType "Bool" [])))
    ,( "<", FunType (ConType "Int" []) (FunType (ConType "Int" []) (ConType "Bool" [])))
{-
    ,("+.", FunType (ConType "Float" []) (FunType (ConType "Float" []) (ConType "Float" [])))
    ,("-.", FunType (ConType "Float" []) (FunType (ConType "Float" []) (ConType "Float" [])))
    ,("*.", FunType (ConType "Float" []) (FunType (ConType "Float" []) (ConType "Float" [])))
    ,("/.", FunType (ConType "Float" []) (FunType (ConType "Float" []) (ConType "Float" [])))
    ,("^.", FunType (ConType "Float" []) (FunType (ConType "Float" []) (ConType "Float" [])))

    ,("==.", FunType (ConType "Float" []) (FunType (ConType "Float" []) (ConType "Bool" [])))
    ,( ">.", FunType (ConType "Float" []) (FunType (ConType "Float" []) (ConType "Bool" [])))
    ,( "<.", FunType (ConType "Float" []) (FunType (ConType "Float" []) (ConType "Bool" [])))
-}
    ,("True", ConType "Bool" [])
    ,("False", ConType "Bool" [])
     
    ,("Nil", ConType "List" [VarType "a"])
    ,("Cons", FunType (VarType "a") (FunType 
                                      (ConType "List" [VarType "a"])
                                      (ConType "List" [VarType "a"])))
                                    
    ]