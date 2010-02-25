module KnownTypes
    (
     addKnownTypes
    ,addTypeSignatures
    ,declsToEn
    )
    where

import Syntax
import TransformMonad(TransformM, transformOk, nullTransform) -- TODO remove nullTransform
import TransformUtils(transformTree, Transformer(..), defTrans)
import TypeUtils(getType, resultingType)
import BuiltIn(EnvType, listType, env0)
import Tools(traceVal)

addKnownTypes :: Program -> TransformM Program
addKnownTypes p@(Program decls) = transformTree
                                  "addKnownTypes: Unable to type (type constructor not found)" 
                                  (defTrans {tExp=adaptExpr, tPat=adaptPattern})
                                  p
    where
      env = env0 ++ (declsToEn decls)
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
      adaptPattern p = Just p

      getLastType (FuncType _ t) = getLastType t
      getLastType t = t
      

lookupWithDefault ::Eq a => a -> [(a, b)] -> b -> b
lookupWithDefault val list def = case lookup val list of
                                   Just res -> res
                                   Nothing -> def

declsToEn :: [Declaration] -> [EnvType]
declsToEn decls = concatMap  build decls
    where
      build :: Declaration -> [EnvType]
      build (DataDcl n params constructors) =
          let 
              buildData typeName constructors = map (buildConstructor typeName) constructors
              buildConstructor typeName (ConDcl name types) = (name, toFunction 
                                                                       (types
                                                                        ++ [ConType
                                                                            typeName
                                                                            polyParams
                                                                           ]))
              toFunction (t:[]) = t
              toFunction (t:ts) = FuncType t (toFunction ts)
              polyParams = map (\param -> (VarType param)) params
          in 
            buildData n constructors
      build d = []

-- addTypeSignature, add declared type signatures to the corresponding pattern
addTypeSignatures :: Program -> TransformM Program
addTypeSignatures (Program decls) = 
    (transformOk $ Program (addTypeSignatureToDeclarations decls))
    >>= addTypeSignatureToLet


addTypeSignatureToLet :: Program -> TransformM Program
addTypeSignatureToLet p@(Program decls) = transformTree
                                  "addKnownTypes: Unable to type (type constructor not found)" 
                                  (defTrans {tExp = adaptExpr})
                                  p
    where
      adaptExpr (LetExp decls e t) = Just $ (LetExp decls' e t)
          where
            decls' = addTypeSignatureToDeclarations decls
      adaptExpr e = Just e


addTypeSignatureToDeclarations decls = map addType decls
    where
      addType (FunBindDcl n pats r t) = (FunBindDcl n pats r (lookupWithDefault n sigs t))
      addType (PatBindDcl (VarPat n t) r) = (PatBindDcl (VarPat n 
                                                                (lookupWithDefault n sigs t)) 
                                             r)
      addType n = n

      sigs = prepareTypeSignatures decls

prepareTypeSignatures :: [Declaration] -> [(Name, Type)]
prepareTypeSignatures decls = concatMap extractFromSig decls
    where
      extractFromSig (TypeSigDcl ns t) = zip ns (replicate (length ns) t)
      extractFromSig _ = []