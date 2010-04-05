{-
  This file is part of HNH.

    HNH is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    HNH is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with HNH.  If not, see <http://www.gnu.org/licenses/>.
    
    Copyright 2010 Francisco Ferreira
-}

module TransformUtils
    (
     transformTree
    ,Transformer(..)
    ,defTrans 
    )
    where

import Syntax
import TransformMonad(TransformM, transformOk, transformError)
import ErrorMonad(ErrorM(..))


-- idM: monadic version of id
idM :: Monad m => a -> m a
idM = return 

data Transformer = Transformer {
  tExp :: Exp -> ErrorM Exp
  ,tPat  :: Pattern -> ErrorM Pattern
  ,tDecls :: [Declaration] -> ErrorM [Declaration]
   -- add other transformers as needed here
  }

defTrans = Transformer idM idM idM

-- transformTree: does a depth first transformation of the program
-- it is not suitable for transformations that require recursive 
-- access to expressions or other constructs

transformTree :: String -> Transformer -> Program -> TransformM Program
transformTree name transform prog@(Program decls) =
    case decls' of
      Success d -> transformOk name $ Program d
      Error msg' -> transformError (name ++ ": " ++ msg')
    where
      decls' =
          do
            decls'' <- tDecls transform $ decls
            mapM adaptDeclaration decls''

      adaptDeclaration (FunBindDcl n pats e) =
          do
            e' <- adaptExp e
            pats' <- mapM (tPat transform) pats
            return $ FunBindDcl n pats' e'

      adaptDeclaration (PatBindDcl pat e) = 
          do
            e' <- adaptExp e
            pat' <- (tPat transform) pat
            return $ PatBindDcl pat' e'

      adaptDeclaration d = return d

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
            decls' <- (tDecls transform) decls
            decls'' <- mapM adaptDeclaration decls'
            e' <- adaptExp e
            (tExp transform) (LetExp decls'' e' t)
            
      adaptExp (IfExp e1 e2 e3 t) =
          do
            e1' <- adaptExp e1
            e2' <- adaptExp e2
            e3' <- adaptExp e3
            (tExp transform) (IfExp e1' e2' e3' t)

      adaptExp (CaseExp es alts t) = 
          do
            es' <- mapM adaptExp es
            alts' <- mapM adaptAlt alts
            (tExp transform) (CaseExp es' alts' t)

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

      adaptAlt (Alternative pats e) =
          do
            e' <- adaptExp e
            pats' <- mapM (tPat transform) pats
            return $ Alternative pats' e'
