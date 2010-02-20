module TransformUtils
    (
     transformExpressions
    )
    where

import Syntax
import TransformMonad(TransformM, transformOk, transformError)
import Control.Monad.Error

transformExpressions :: String -> (Expr -> Maybe Expr) -> Program -> TransformM Program
transformExpressions msg transform prog@(Program decls) = 
    case decls' of
      Just d -> transformOk $ Program d
      Nothing -> transformError msg
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
            e' <- transform e
            return $ UnGuardedRhs e'
      adaptRhs (GuardedRhs guards) =
          do
            guards' <- mapM adaptGuard guards
            return $ GuardedRhs guards'

      adaptGuard (Guard e1 e2) =
          do
            e1' <- transform e1
            e2' <- transform e2
            return $ Guard e1' e2'

