module Types
    (
     addBuiltInTypes
    ,Env
    ,listType
    )
    where

import Syntax
import TransformMonad(TransformM)
import TransformUtils(transformExpressions)

type Env = (Name, Type)


addBuiltInTypes :: Program -> TransformM Program
addBuiltInTypes = transformExpressions
                  "Unable to find some operators" --TODO ??
                  adaptExpr
    where
      adaptExpr e = Just e

chain :: Maybe a -> (a -> Maybe b) -> Maybe b
chain (Just a) f = f a
chain Nothing _ = Nothing

{-
buildTypeDic :: Program -> [(Name, [Name], Type)] -- (type name, polymorphic parameters, type)
buildTypeDic (Program decls) = concatMap getType decls
    where
      getType :: Program -> (Name, Type)
      getType (TypeDcl n p t) = (n, p, t)
      getType (DataDcl n p 
-}



--- BuiltIns

listType = ConType "List" -- The defaultType of list

env0 :: [Env] -- the initial environment, containing all the builin functions
env0 = 
    [("+", FuncType (ConType "Int") (ConType "Int"))
    ,("-", FuncType (ConType "Int") (ConType "Int"))
    ,("*", FuncType (ConType "Int") (ConType "Int"))
    ,("/", FuncType (ConType "Int") (ConType "Int"))
    ,("^", FuncType (ConType "Int") (ConType "Int"))

    ,("+.", FuncType (ConType "Float") (ConType "Float"))
    ,("-.", FuncType (ConType "Float") (ConType "Float"))
    ,("*.", FuncType (ConType "Float") (ConType "Float"))
    ,("/.", FuncType (ConType "Float") (ConType "Float"))
    ,("^.", FuncType (ConType "Float") (ConType "Float"))

    ,("==", FuncType (ConType "Bool") (ConType "Bool"))
    ,( ">", FuncType (ConType "Bool") (ConType "Bool"))
    ,( "<", FuncType (ConType "Bool") (ConType "Bool"))
    ]