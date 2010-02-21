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
import TypeUtils(getType, resultingType)
import Tools(traceVal)


type Env = (Name, Type)


addBuiltInTypes :: Program -> TransformM Program
addBuiltInTypes p@(Program decls) = transformExpressions
                                    "addBuiltInTypes: Unable to statically type" 
                                    adaptExpr
                                    p
    where
      env = {-traceVal $-} env0 ++ (processDeclarations decls) -- TODO complete this here or on in another phase
      adaptExpr (VarExp n _) = Just $ VarExp n (lookupWithDefault n env UnknownType)
      adaptExpr (ConExp n _) = Just $ ConExp n (lookupWithDefault n env UnknownType)
      adaptExpr (MinusExp _ _) = Nothing -- we are not supposed to have these at this point
      adaptExpr (MinusFloatExp _ _) = Nothing 
      adaptExpr (InfixOpExp _ _) = Nothing
      adaptExpr (FExp e1 e2 _) = Just $ FExp e1 e2 (resultingType (getType e1))
      adaptExpr (LetExp decls e t) = Just $ LetExp decls e (getType e)
      adaptExpr e = Just e
      

lookupWithDefault ::Eq a => a -> [(a, b)] -> b -> b
lookupWithDefault val list def = case lookup val list of
                                   Just res -> res
                                   Nothing -> def

{-
buildTypeDic :: Program -> [(Name, [Name], Type)] -- (type name, polymorphic parameters, type)
buildTypeDic (Program decls) = concatMap getType decls
    where
      getType :: Program -> (Name, Type)
      getType (TypeDcl n p t) = (n, p, t)
      getType (DataDcl n p 
-}

processDeclarations :: [Declaration] -> [Env]
processDeclarations decls = concatMap  build decls
    where
      build :: Declaration -> [Env]
      build (DataDcl n params constructors) = buildData n constructors
      build d = []
      buildData :: Name -> [ConstructorDeclaration] -> [Env]
      buildData typeName constructors = map buildConstructor constructors
          where
            buildConstructor (ConsDcl name types) = (name, toFunction (types++[ConType typeName]))
            toFunction (t:[]) = t
            toFunction (t:ts) = FuncType t (toFunction ts)

--- BuiltIns -- TODO maybe it will be better to move this away

listType = ConType "List" -- The defaultType of list

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