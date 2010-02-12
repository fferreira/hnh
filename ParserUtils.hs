module ParserUtils where --TODO add right exports here

import Syntax
import ParserMonad

-- Check Equation Syntax

checkValDef :: Expr -> Rhs -> [Declaration] -> ParserM Declaration
checkValDef lhs rhs whereBinds =
    case isFunLhs lhs [] of
	 Just (f,es) -> do
			ps <- mapM checkPattern es
			return (FunBindDcl [Match f ps rhs{- whereBinds-} []])
         Nothing     -> do
			lhs <- checkPattern lhs
			return (PatBind lhs rhs whereBinds)

-- A variable binding is parsed as an HsPatBind.

isFunLhs :: Expr -> [Expr] -> Maybe (Name, [Expr])
isFunLhs (InfixOp l op r) es = Just (op, l:r:es)
isFunLhs (FExp (VarExp f) e) es = Just (f, e:es)
isFunLhs (FExp (ParensExp f) e) es = isFunLhs f (e:es)
isFunLhs (FExp f e) es = isFunLhs f (e:es)
isFunLhs _ _ = Nothing

-----------------------------------------------------------------------------
-- Checking Patterns.

-- We parse patterns as expressions and check for valid patterns below,
-- converting the expression into a pattern at the same time.

checkPattern :: Expr -> ParserM Pattern
checkPattern = error "no checkPattern yet"
{-
checkPattern e = checkPat e []

checkPat :: Expr -> [Patttern] -> ParseM Patttern
checkPat (ConExp c) args = return (HsPApp c args)
checkPat (HsApp f x) args = do
	x <- checkPat x []
	checkPat f (x:args)
checkPat e [] = case e of
	HsVar (UnQual x)   -> return (HsPVar x)
	HsLit l            -> return (HsPLit l)
	HsInfixApp l op r  -> do
			      l <- checkPat l []
			      r <- checkPat r []
			      case op of
				 HsQConOp c -> return (HsPInfixApp l c r)
				 _ -> patFail
	HsTuple es         -> do
			      ps <- mapM (\e -> checkPat e []) es
			      return (HsPTuple ps)
	HsList es	   -> do
			      ps <- mapM (\e -> checkPat e []) es
			      return (HsPList ps)
	HsParen e	   -> do
			      p <- checkPat e []
			      return (HsPParen p)
	HsAsPat n e	   -> do
			      p <- checkPat e []
			      return (HsPAsPat n p)
	HsWildCard	   -> return HsPWildCard
	HsIrrPat e	   -> do
			      p <- checkPat e []
			      return (HsPIrrPat p)
	HsRecConstr c fs   -> do
			      fs <- mapM checkPatField fs
			      return (HsPRec c fs)
	HsNegApp (HsLit l) -> return (HsPNeg (HsPLit l))
	_ -> patFail

checkPat _ _ = patFail

checkPatField :: HsFieldUpdate -> P HsPatField
checkPatField (HsFieldUpdate n e) = do
	p <- checkPat e []
	return (HsPFieldPat n p)

patFail :: P a
patFail = fail "Parse error in pattern"
-}