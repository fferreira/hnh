module TransformUtils
    (
     transformTree
    ,Transformer(..)
    ,idM
    )
    where

import Syntax
import TransformMonad(TransformM, transformOk, transformError)
import Control.Monad.Error

-- idM: monadic version of id
idM :: Monad m => a -> m a
idM = return 

data Transformer = Transformer {
                   expression :: (Exp -> Maybe Exp)
                  ,pattern :: (Pattern -> Maybe Pattern)
                  -- TODO maybe add other transformers here
    }

-- transformTree: does a depth first transformation of the program
-- it is not suitable for transformations that require recursive 
-- access to expressions or other constructs

transformTree :: String -> Transformer -> Program -> TransformM Program
transformTree msg transform prog@(Program decls) = 
    case decls' of
      Just d -> transformOk $ Program d
      Nothing -> transformError msg
    where
      decls' = mapM adaptDeclaration decls
      adaptDeclaration (FunBindDcl n pats rhs) =
          do
            rhs' <- adaptRhs rhs
            pats' <- mapM (pattern transform) pats
            return $ FunBindDcl n pats' rhs'

      adaptDeclaration (PatBindDcl pat rhs) = 
          do
            rhs' <- adaptRhs rhs
            pat' <- (pattern transform) pat
            return $ PatBindDcl pat' rhs'

      adaptDeclaration d = Just d

      adaptRhs (UnGuardedRhs e) = 
          do
            e' <- adaptExp e
            return $ UnGuardedRhs e'
      adaptRhs (GuardedRhs guards) =
          do
            guards' <- mapM adaptGuard guards
            return $ GuardedRhs guards'

      adaptGuard (Guard e1 e2) =
          do
            e1' <- adaptExp e1
            e2' <- adaptExp e2
            return $ Guard e1' e2'

      adaptExp (InfixOpExp opEx t) =
          do
            opEx' <- adaptOpExp opEx
            (expression transform) (InfixOpExp opEx' t)

      adaptExp (FExp e1 e2 t) =
          do
            e1' <- adaptExp e1
            e2' <- adaptExp e2
            (expression transform) (FExp e1' e2' t)

      adaptExp (MinusExp e t) =
          do
            e' <- adaptExp e
            (expression transform) (MinusExp e' t)

      adaptExp (MinusFloatExp e t) =
          do
            e' <- adaptExp e
            (expression transform) (MinusFloatExp e' t)

      adaptExp (LambdaExp pats e t) = 
      -- the patterns contain noting but names
          do
            e' <- adaptExp e
            pats' <- mapM (pattern transform) pats
            (expression transform) (LambdaExp pats' e' t)

      adaptExp (LetExp decls e t) =
          do
            decls' <- mapM adaptDeclaration decls
            e' <- adaptExp e
            (expression transform) (LetExp decls' e' t)
            
      adaptExp (IfExp e1 e2 e3 t) =
          do
            e1' <- adaptExp e1
            e2' <- adaptExp e2
            e3' <- adaptExp e3
            (expression transform) (IfExp e1' e2' e3' t)

      adaptExp (CaseExp e alts t) = 
          do
            e' <- adaptExp e
            alts' <- mapM adaptAlt alts
            (expression transform) (CaseExp e' alts' t)

      adaptExp (ParensExp e t) =
          do
            e' <- adaptExp e
            (expression transform) (ParensExp e' t)

      adaptExp (TupleExp exps t) =
          do
            exps' <- mapM adaptExp exps
            (expression transform) (TupleExp exps' t)

      adaptExp (ListExp exps t) =
          do
            exps' <- mapM adaptExp exps
            (expression transform) (ListExp exps' t)

      adaptExp e = (expression transform) e
                    
      adaptOpExp (LeafExp e) =
          do
            e' <- adaptExp e
            return $ LeafExp e'

      adaptOpExp (Op o e1 e2) =
          do
            e1' <- adaptOpExp e1
            e2' <- adaptOpExp e2
            return $ Op o e1' e2'

      adaptAlt (Alternative pat e) =
          do
            e' <- adaptExp e
            pat' <- (pattern transform) pat
            return $ Alternative pat' e'
