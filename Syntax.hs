module Syntax
    (
     Program(..)
    ,Declaration(..)
    ,Name
    ,Operator
    ,LiteralValue(..)
    ,Rhs(..)
    ,Guard(..)
    ,Precedence
    ,Associativity(..)
    ,Type(..)
    ,ConstructorDeclaration(..)
    ,OpExpr(..)
    ,Expr(..)
    ,Pattern(..)
    ,Alternative(..)
    )
    where

import Text.PrettyPrint.Leijen -- requires wl-pprint installed (available in cabal)

data Program = Program [Declaration] deriving(Show, Eq)

--- Names & Literals

type Name = String
type Operator = String -- TODO should be something more specific ?

data LiteralValue
    = LiteralInt Int
    | LiteralFloat Double
    | LiteralString String
    | LiteralChar String -- this is a string instead of a char as per the lexer
      deriving (Show, Eq)

--- Declarations

data Declaration
    = TypeDcl Name [Name] Type 
    | DataDcl Name [Name] [ConstructorDeclaration] 
    | TypeSigDcl [Name] Type 
    | FixityDcl Associativity Precedence [Operator]
    | FunBindDcl Name [Pattern] Rhs
    | PatBindDcl Pattern Rhs
      deriving(Show, Eq)

data Rhs
    = UnGuardedRhs Expr
    | GuardedRhs [Guard]
      deriving(Show, Eq)

data Guard
    = Guard Expr Expr -- first Expr evaluates to Bool, the second is the function body
      deriving(Show, Eq)

type Precedence  = Int

data Associativity -- TODO change name to fixity ?
    = NonAssoc
    | LeftAssoc
    | RightAssoc
      deriving(Show, Eq)

data Type
    = FuncType Type Type
    | TupleType [Type]
    | AppType Type Type -- TODO when is this??
    | VarType Name      -- for parametric types
    | ConsType Name     -- the constructor of the type
    | UnknownType
      deriving(Show, Eq)

data ConstructorDeclaration -- No support for named field types
    = ConsDcl Name [Type]
      deriving(Show, Eq)

--- Expressions & Patterns

data OpExpr
    = LeafExp Expr
    | Op Operator OpExpr OpExpr
      deriving (Show, Eq)

data Expr -- TODO add a switch statement with expressions?
    = VarExp Name Type
    | ConExp Name Type
    | LitExp LiteralValue Type
--    | InfixOpExp Expr Operator Expr Type
    | InfixOpExp OpExpr Type
    | FExp Expr Expr Type -- function application expression          
    | MinusExp Expr Type
    | LambdaExp [Pattern] Expr Type
    | LetExp [Declaration] Expr Type
    | IfExp Expr Expr Expr Type
    | CaseExp Expr [Alternative] Type
    | ParensExp Expr Type
    | TupleExp [Expr] Type -- a tuple of expresions
    | ListExp [Expr] Type  -- a list of expresions
    | ArithSeqExp Expr (Maybe Expr) Expr Type -- from, increment, to (no infinite sequences)
      deriving (Show, Eq)

data Pattern
    = VarPat Name
    | AsPat Name Pattern
    | ConsPat Name [Pattern] -- a type constructor 
    | LitPat LiteralValue
    | ListPat [Pattern]
    | HeadTailPat Name Name -- x:xs pattern type
    | TuplePat [Pattern]
    | WildcardPat
      deriving (Show, Eq)

data Alternative
    = Alternative Pattern Expr
    deriving (Show, Eq)

-- Pretty printing support (very important for debugability)

instance Pretty Program where
    pretty (Program d) = vsep (map pretty d)
    prettyList ps = vsep (map pretty ps) 

instance Pretty LiteralValue where
    pretty (LiteralInt i) = pretty i
    pretty (LiteralFloat f) = pretty f
    pretty (LiteralString s) = pretty s
    pretty (LiteralChar c) = pretty c

instance Pretty Declaration where
    pretty (TypeDcl n ns t) = pretty "type " <> pretty n <> pretty ns <> equals <> pretty t
    pretty (DataDcl n ns ts) = pretty "data " <> pretty n <> pretty ns <> equals <!> pretty ts
    pretty (TypeSigDcl ns t) = pretty ns <> colon <> colon <> pretty t
    pretty (FixityDcl NonAssoc p ops) = pretty "infix"<+> pretty p <+> pretty ops
    pretty (FixityDcl LeftAssoc p ops) = pretty "infixl"<+> pretty p <+> pretty ops
    pretty (FixityDcl RightAssoc p ops) = pretty "infixr"<+> pretty p <+> pretty ops
    pretty (FunBindDcl n ps r) = pretty n <> pretty ps <> equals <> pretty r
    pretty (PatBindDcl p r) = pretty p <> equals <> pretty r

instance Pretty Rhs where
    pretty (UnGuardedRhs e) = pretty e
    pretty (GuardedRhs g) = pretty g

instance Pretty Guard where
    pretty (Guard e1 e2) = pretty e1 <> equals <> pretty e2

instance Pretty Type where
    pretty (FuncType t1 t2) = pretty t1 <> pretty "->" <> pretty t2
    pretty (TupleType t) = pretty t
    pretty (AppType t1 t2) = pretty t2 <> pretty "[ap]" <> pretty t2
    pretty (VarType n) = pretty n
    pretty (ConsType n) = pretty n
    pretty (UnknownType) = pretty "?"

instance Pretty ConstructorDeclaration where
    pretty (ConsDcl n t) = pretty n <!> pretty t

instance Pretty OpExpr where
    pretty (LeafExp e) = pretty e
    pretty (Op o e1 e2) = parens $ pretty o <+> pretty e1 <+> pretty e2
instance Pretty Expr where
    pretty (VarExp n t) = parens $ pretty n -- <> comma <+> pretty t
    pretty (ConExp n t) = parens $ pretty n -- <> comma <+> pretty t
    pretty (LitExp v t) = parens $ pretty v -- <> comma <+> pretty t
    {-pretty (InfixOpExp e1 op e2 t) = parens $ 
                                     pretty e1 
                                                <> pretty op 
                                                <> pretty e2 
                                                <> comma <+> pretty t-}
    pretty (InfixOpExp e t) = parens $ pretty e -- <> comma <+> pretty t
    pretty (FExp e1 e2 t) = parens $ pretty e1 <//> pretty e2 -- <> comma <+> pretty t
    pretty (MinusExp e t) = parens $ pretty "~" <> pretty e -- <> comma <+> pretty t
    pretty (LambdaExp p e t) = parens $ 
                            pretty "\\" 
                                       <> pretty p <> pretty "->"
                                       <> pretty e -- <> comma <+> pretty t
    pretty (TupleExp e t) = parens $ pretty "#" <> pretty e -- <> comma <+> pretty t
    pretty (ListExp e t) = parens $ pretty e -- <> comma <+> pretty t
    pretty (ArithSeqExp e1 e2 e3 t) = parens $ brackets $ pretty e1
                                      <> comma <> pretty e2 <> dot <> dot 
                                      <> pretty e3 -- <> comma <+> pretty t

instance Pretty Pattern where
    pretty (VarPat n)    = pretty n
    pretty (AsPat n p)   = pretty n <> pretty "@" <> pretty p
    pretty (ConsPat n p) = pretty n <!> pretty p
    pretty (LitPat lit)  = pretty lit
    pretty (TuplePat t)  = pretty "#" <> pretty t
    pretty (ListPat l)   = pretty l
    pretty (HeadTailPat n1 n2) = pretty n1 <> colon <> pretty n2
    pretty (WildcardPat) = pretty '_'

instance Pretty Alternative where
    pretty (Alternative p e) = pretty p <> pretty " -> " <!> pretty e




(<!>) a b = a <> space <> (hang 4) b

