module KnownTypes
    (
     addKnownTypes
    )
    where

import Syntax
import TransformMonad(TransformM)
import TransformUtils(transformTree, Transformer(..), idM)
import TypeUtils(getType, resultingType)
import BuiltIn(Env, listType, env0)
import Tools(traceVal)

addKnownTypes :: Program -> TransformM Program
addKnownTypes p@(Program decls) = transformTree
                                  "addKnownTypes: Unable to type (type constructor not found)" 
                                  (Transformer adaptExpr adaptPattern)
                                  p
    where
      env = env0 ++ (processDeclarations decls)
      adaptExpr (VarExp n UnknownType) = Just $ VarExp n (lookupWithDefault n env UnknownType)
      adaptExpr (ConExp n _) = 
          do
            t <- lookup n env -- Fails when the constructor does not exist
            return $ ConExp n t
      adaptExpr (MinusExp _ _) = Nothing -- we are not supposed to have these at this point
      adaptExpr (MinusFloatExp _ _) = Nothing 
      adaptExpr (InfixOpExp _ _) = Nothing
      adaptExpr (FExp e1 e2 _) = Just $ FExp e1 e2 (resultingType (getType e1))
      adaptExpr (LetExp decls e _) = Just $ LetExp decls e (getType e)
      -- if a ParensExp has type annotations it won't take the type of its subexpression
      adaptExpr (ParensExp e UnknownType) = Just $ ParensExp e (getType e) 
      adaptExpr e = Just e

      adaptPattern (TuplePat vars _) = Just $ 
                                       TuplePat vars 
                                                (TupleType (replicate (length vars) UnknownType))

      adaptPattern (ConPat name params _) =
          do
            t <- lookup name env -- Fails when the constructor does not exist
            return (ConPat name params (getLastType t))
      adaptPattern (ListPat names _) = Just $ ListPat names listType
      adaptPattern (HeadTailPat h t _) = Just $ HeadTailPat h t listType
      adaptPattern p = Just p

      getLastType (FuncType _ f@(FuncType _ _)) = getLastType f
      getLastType t = t
      

lookupWithDefault ::Eq a => a -> [(a, b)] -> b -> b
lookupWithDefault val list def = case lookup val list of
                                   Just res -> res
                                   Nothing -> def

processDeclarations :: [Declaration] -> [Env]
processDeclarations decls = concatMap  build decls
    where
      build :: Declaration -> [Env]
      build (DataDcl n params constructors) = buildData n constructors
      build d = []
      buildData :: Name -> [ConstructorDeclaration] -> [Env]
      buildData typeName constructors = map buildConstructor constructors
          where
            buildConstructor (ConDcl name types) = (name, toFunction (types++[ConType typeName]))
            toFunction (t:[]) = t
            toFunction (t:ts) = FuncType t (toFunction ts)
