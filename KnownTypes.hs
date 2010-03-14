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
                                  "addKnownTypes"
                                  (defTrans {tExp=adaptExpr, tPat=adaptPattern})
                                  p
    where
      env = env0 ++ (declsToEn decls)
      
      adaptExpr :: Monad m => Exp -> m Exp
      adaptExpr (VarExp n UnknownType) = return $ VarExp n (lookupWithDefault n env UnknownType)
      adaptExpr (ConExp n _) = 
          do
            t <- lookupM n env -- Fails when the constructor does not exist
            return $ ConExp n t
      adaptExpr (MinusExp _ _) = fail "Unexpected" -- we are not supposed to have these at this point
      adaptExpr (MinusFloatExp _ _) = fail "Unexpected"
      adaptExpr (InfixOpExp _ _) = fail "Unexpected"
      adaptExpr (FExp e1 e2 _) = return $ FExp e1 e2 (resultingType (getType e1))
      adaptExpr (LetExp decls e _) = return $ LetExp decls e (getType e)
      -- if a ParensExp has type annotations it won't take the type of its subexpression
      adaptExpr (ParensExp e UnknownType) = return $ ParensExp e (getType e) 
      adaptExpr e = return e

      adaptPattern (TuplePat vars _) = return $ 
                                       TuplePat vars 
                                                (TupleType (replicate (length vars) UnknownType))

      adaptPattern (ConPat name params _) =
          do
            t <- lookupM name env -- Fails when the constructor does not exist
            return (ConPat name params (getLastType t))
      adaptPattern p = return p

      getLastType (FuncType _ t) = getLastType t
      getLastType t = t
      
lookupM :: (Monad m, Eq a, Show a) => a -> [(a, b)] -> m b
lookupM e l = 
  let 
    res = lookup e l 
  in 
   case res of 
     (Just res') -> return res'
     Nothing -> fail ("lookup of " ++ show e ++ " failed")

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
    (transformOk "addTypeSignatures" $ Program (addTypeSignatureToDeclarations decls))
    >>= addTypeSignatureToLet


addTypeSignatureToLet :: Program -> TransformM Program
addTypeSignatureToLet p@(Program decls) = transformTree
                                  "addKnownTypes: Unable to type (type constructor not found)" 
                                  (defTrans {tExp = adaptExpr})
                                  p
    where
      adaptExpr (LetExp decls e t) = return $ (LetExp decls' e t)
          where
            decls' = addTypeSignatureToDeclarations decls
      adaptExpr e = return e


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