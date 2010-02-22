module InferTypes
    (
     performTypeInference
    )
    where

import Syntax
import BuiltIn(Env, env0)
import TransformMonad (TransformM, nullTransform) --TODO nullTransform has to go!

performTypeInference :: Program -> TransformM Program
performTypeInference = nullTransform


addMetaTypes :: Monad m => Name -> [Env] -> [Declaration] -> m ([Declaration], [Env])
addMetaTypes = undefined

findDeclaration :: Monad m => Name -> [Declaration] -> m Declaration
findDeclaration n decls = 
    case filter (\d -> elem n (namesDeclared d)) decls of
      x:[] -> return x
      _ -> fail "not found, or multiple declarations"

namesDeclared :: Declaration -> [Name]
namesDeclared (FunBindDcl n _ _ _) = [n]
namesDeclared (PatBindDcl p _) = namesFromPattern p

namesFromPattern :: Pattern -> [Name]
namesFromPattern (VarPat n _) = [n]
namesFromPattern (ConPat _ ns _) = ns
namesFromPattern (HeadTailPat n1 n2 _) = [n1, n2]
namesFromPattern (TuplePat ns _) = ns
namesFromPattern (WildcardPat _) = []

getRhsType :: Rhs -> Type
getRhsType = undefined


{-
infer env (FunBindDcl n pats r) = undefined
infer env (PatBindDcl pat r) = undefined
infer env d = undefined
-}