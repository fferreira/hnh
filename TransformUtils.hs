module TransformUtils
    (
     transformTree
    ,Transformer(..)
    ,defTrans 
    )
    where

import Syntax
import TransformMonad(TransformM, transformOk, transformError)
import Control.Monad.Error

-- idM: monadic version of id
idM :: Monad m => a -> m a
idM = return 

data Transformer = Transformer {
                   tExp :: Exp -> Maybe Exp
                  ,tPat  :: Pattern -> Maybe Pattern
                  ,tDecls :: [Declaration] -> Maybe [Declaration]
                  -- TODO maybe add other transformers here
    }

defTrans = Transformer idM idM idM

-- transformTree: does a depth first transformation of the program
-- it is not suitable for transformations that require recursive 
-- access to expressions or other constructs

transformTree :: String -> Transformer -> Program -> TransformM Program
transformTree msg transform prog@(Program decls) = 
    case decls' of  -- TODO USE ErrorM!!!!
      Just d -> transformOk $ Program d
      Nothing -> transformError msg
    where
      decls' =
          do
            decls'' <- tDecls transform $ decls
            mapM adaptDeclaration decls''

      adaptDeclaration (FunBindDcl n pats e t) =
          do
            e' <- adaptExp e
            pats' <- mapM (tPat transform) pats
            return $ FunBindDcl n pats' e' t

      adaptDeclaration (PatBindDcl pat e) = 
          do
            e' <- adaptExp e
            pat' <- (tPat transform) pat
            return $ PatBindDcl pat' e'

      adaptDeclaration d = Just d

      adaptExp (InfixOpExp opEx t) =
          do
            opEx' <- adaptOpExp opEx
            (tExp transform) (InfixOpExp opEx' t)

      adaptExp (FExp e1 e2 t) =
          do
            e1' <- adaptExp e1
            e2' <- adaptExp e2
            (tExp transform) (FExp e1' e2' t)

      adaptExp (MinusExp e t) =
          do
            e' <- adaptExp e
            (tExp transform) (MinusExp e' t)

      adaptExp (MinusFloatExp e t) =
          do
            e' <- adaptExp e
            (tExp transform) (MinusFloatExp e' t)

      adaptExp (LambdaExp pats e t) = 
      -- the patterns contain noting but names
          do
            e' <- adaptExp e
            pats' <- mapM (tPat transform) pats
            (tExp transform) (LambdaExp pats' e' t)

      adaptExp (LetExp decls e t) =
          do
            decls' <- mapM adaptDeclaration decls
            e' <- adaptExp e
            (tExp transform) (LetExp decls' e' t)
            
      adaptExp (IfExp e1 e2 e3 t) =
          do
            e1' <- adaptExp e1
            e2' <- adaptExp e2
            e3' <- adaptExp e3
            (tExp transform) (IfExp e1' e2' e3' t)

      adaptExp (CaseExp e alts t) = 
          do
            e' <- adaptExp e
            alts' <- mapM adaptAlt alts
            (tExp transform) (CaseExp e' alts' t)

      adaptExp (ParensExp e t) =
          do
            e' <- adaptExp e
            (tExp transform) (ParensExp e' t)

      adaptExp (TupleExp exps t) =
          do
            exps' <- mapM adaptExp exps
            (tExp transform) (TupleExp exps' t)

      adaptExp (ListExp exps t) =
          do
            exps' <- mapM adaptExp exps
            (tExp transform) (ListExp exps' t)

      adaptExp e = (tExp transform) e
                    
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
            pat' <- (tPat transform) pat
            return $ Alternative pat' e'
