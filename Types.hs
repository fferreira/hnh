module Types
    (
     addBuiltInTypes
    ,Env
    ,listType
    )
    where

import Syntax
import TransformMonad(TransformM, transformOk, transformError)
import TransformUtils(transformExpressions)

type Env = (Name, Type)

abt :: Program -> TransformM Program
abt p = transformExpressions "unable" transform p
    where
      transform = undefined


addBuiltInTypes :: Program -> TransformM Program
addBuiltInTypes  prog@(Program decls) = 
    case decls' of
      Just d -> transformOk $ Program d
      Nothing -> transformError "Unable to find some operators"
    where
      decls' = mapM adaptDeclaration decls
      adaptDeclaration (FunBindDcl n pats rhs) =
          do
            rhs' <- adaptRhs rhs
            return $ FunBindDcl n pats rhs'
      adaptDeclaration (PatBindDcl pat rhs) = 
          do
            rhs' <- adaptRhs rhs
            return $ PatBindDcl pat rhs'
      adaptDeclaration d = Just d

      adaptRhs (UnGuardedRhs e) = 
          do
            e' <- adaptExpr e
            return $ UnGuardedRhs e'
      adaptRhs (GuardedRhs guards) =
          do
            guards' <- mapM adaptGuard guards
            return $ GuardedRhs guards'

      adaptGuard (Guard e1 e2) =
          do
            e1' <- adaptExpr e1
            e2' <- adaptExpr e2
            return $ Guard e1' e2'

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