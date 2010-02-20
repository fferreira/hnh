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
import ParserUtils(getType, resultingType)

type Env = (Name, Type)


addBuiltInTypes :: Program -> TransformM Program
addBuiltInTypes = transformExpressions
                  "addBuiltInTypes: Unable to statically type" 
                  adaptExpr
    where
      env = env0 -- TODO complete this
      adaptExpr (VarExp n _) =
          do
            t <- lookup n env
            return $ VarExp n t
      adaptExpr (FExp e1 e2 _) = 
          do
            e1' <- adaptExpr e1
            e2' <- adaptExpr e2
            t <- resultingType (getType e1')
            return $ FExp e1' e2' t
      adaptExpr e = Just e
      

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
    [("+", FuncType (ConType "Int") (FuncType (ConType "Int") (ConType "Int")))
    ,("-", FuncType (ConType "Int") (FuncType (ConType "Int") (ConType "Int")))
    ,("*", FuncType (ConType "Int") (FuncType (ConType "Int") (ConType "Int")))
    ,("/", FuncType (ConType "Int") (FuncType (ConType "Int") (ConType "Int")))
    ,("^", FuncType (ConType "Int") (FuncType (ConType "Int") (ConType "Int")))

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
    ]