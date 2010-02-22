module BuiltIn
    (
     Env
    ,listType
    ,env0
    )
    where

import Syntax

type Env = (Name, Type)

listType = ConType "List" -- The defaultType of list --TODO add polymorphism

env0 :: [Env] -- the initial environment, containing all the builin functions
env0 = 
    [("+", FuncType (ConType "Int") (FuncType (ConType "Int") (ConType "Int")))
    ,("-", FuncType (ConType "Int") (FuncType (ConType "Int") (ConType "Int")))
    ,("*", FuncType (ConType "Int") (FuncType (ConType "Int") (ConType "Int")))
    ,("/", FuncType (ConType "Int") (FuncType (ConType "Int") (ConType "Int")))
    ,("^", FuncType (ConType "Int") (FuncType (ConType "Int") (ConType "Int")))
    ,("~", FuncType (ConType "Int") (ConType "Int"))

    ,("==", FuncType (ConType "Int") (FuncType (ConType "Int") (ConType "Bool")))
    ,( ">", FuncType (ConType "Int") (FuncType (ConType "Int") (ConType "Bool")))
    ,( "<", FuncType (ConType "Int") (FuncType (ConType "Int") (ConType "Bool")))

    ,("+.", FuncType (ConType "Float") (FuncType (ConType "Float") (ConType "Float")))
    ,("-.", FuncType (ConType "Float") (FuncType (ConType "Float") (ConType "Float")))
    ,("*.", FuncType (ConType "Float") (FuncType (ConType "Float") (ConType "Float")))
    ,("/.", FuncType (ConType "Float") (FuncType (ConType "Float") (ConType "Float")))
    ,("^.", FuncType (ConType "Float") (FuncType (ConType "Float") (ConType "Float")))

    ,("==.", FuncType (ConType "Float") (FuncType (ConType "Float") (ConType "Bool")))
    ,( ">.", FuncType (ConType "Float") (FuncType (ConType "Float") (ConType "Bool")))
    ,( "<.", FuncType (ConType "Float") (FuncType (ConType "Float") (ConType "Bool")))

    ,("True", ConType "Bool")
    ,("False", ConType "Bool")
    ]