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
            opEx' <- adaptOpExpr opEx
            transform (InfixOpExp opEx' t)

      adaptExp (FExp e1 e2 t) =
          do
            e1' <- adaptExp e1
            e2' <- adaptExp e2
            transform (FExp e1' e2' t)

      adaptExp (MinusExp e t) =
          do
            e' <- adaptExp e
            transform (MinusExp e' t)

      adaptExp (MinusFloatExp e t) =
          do
            e' <- adaptExp e
            transform (MinusFloatExp e' t)

      adaptExp (LambdaExp pats e t) = -- the patterns contain notinb but names and literals
          do
            e' <- adaptExp e
            transform (LambdaExp pats e' t)

      adaptExp (LetExp decls e t) =
          do
            decls' <- mapM adaptDeclaration decls
            e' <- adaptExp e
            transform (LetExp decls' e' t)
            
      adaptExp (IfExp e1 e2 e3 t) =
          do
            e1' <- adaptExp e1
            e2' <- adaptExp e2
            e3' <- adaptExp e3
            transform (IfExp e1' e2' e3' t)

      adaptExp (CaseExp e alts t) = 
          do
            e' <- adaptExp e
            alts' <- mapM adaptAlt alts
            transform (CaseExp e' alts' t)

      adaptExp (ParensExp e t) =
          do
            e' <- adaptExp e
            transform (ParensExp e' t)

      adaptExp (TupleExp exps t) =
          do
            exps' <- mapM adaptExp exps
            transform (TupleExp exps' t)

      adaptExp (ListExp exps t) =
          do
            exps' <- mapM adaptExp exps
            transform (ListExp exps' t)

      adaptExp e = transform e
                    
      adaptOpExpr (LeafExp e) =
          do
            e' <- adaptExp e
            return $ LeafExp e'

      adaptOpExpr (Op o e1 e2) =
          do
            e1' <- adaptOpExpr e1
            e2' <- adaptOpExpr e2
            return $ Op o e1' e2'

      adaptAlt (Alternative p e) =
          do
            e' <- adaptExp e
            return $ Alternative p e'
