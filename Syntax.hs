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
    ,OpExp(..)
    ,Exp(..)
    ,Pattern(..)
    ,Alternative(..)
    )
    where

import Text.PrettyPrint.Leijen -- requires wl-pprint installed (available in cabal)

data Program = Program [Declaration] deriving(Show, Eq)

--- Names & Literals

type Name = String
type Operator = String

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
    = UnGuardedRhs Exp
    | GuardedRhs [Guard]
      deriving(Show, Eq)

data Guard
    = Guard Exp Exp -- first Exp evaluates to Bool, the second is the function body
      deriving(Show, Eq)

type Precedence  = Int

data Associativity
    = NonAssoc
    | LeftAssoc
    | RightAssoc
      deriving(Show, Eq)

data Type -- for type declarations
    = FuncType Type Type
    | TupleType [Type]
    | AppType Type Type  -- a constructor followed by its parameters
    | VarType Name      -- for parametric types
    | ConType Name     -- the constructor of the type
    | UnknownType
      deriving(Show, Eq)

data ConstructorDeclaration -- No support for named field types
    = ConsDcl Name [Type]
      deriving(Show, Eq)

--- Expressions & Patterns

data OpExp
    = LeafExp Exp
    | Op Operator OpExp OpExp
      deriving (Show, Eq)

data Exp -- TODO rename to Exp!
    = VarExp Name Type
    | ConExp Name Type
    | LitExp LiteralValue Type
    | InfixOpExp OpExp Type
    | FExp Exp Exp Type -- function application expression          
    | MinusExp Exp Type
    | MinusFloatExp Exp Type
    | LambdaExp [Pattern] Exp Type
    | LetExp [Declaration] Exp Type
    | IfExp Exp Exp Exp Type
    | CaseExp Exp [Alternative] Type
    | ParensExp Exp Type
    | TupleExp [Exp] Type -- a tuple of expresions
    | ListExp [Exp] Type  -- a list of expresions
      deriving (Show, Eq)

data Pattern
    = VarPat Name
    | ConPat Name [Name] -- a type constructor 
    | ListPat [Name]
    | HeadTailPat Name Name -- x:xs pattern type
    | TuplePat [Name]
    | WildcardPat
      deriving (Show, Eq)

data Alternative
    = Alternative Pattern Exp
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
    pretty (FuncType t1 t2) = parens $ pretty t1 <> pretty "->" <> pretty t2
    pretty (TupleType t) = pretty t
    pretty (AppType t1 t2) = pretty t1 <> pretty " of " <> pretty t2
    pretty (VarType n) = pretty n
    pretty (ConType n) = pretty n
    pretty (UnknownType) = pretty "?"

instance Pretty ConstructorDeclaration where
    pretty (ConsDcl n t) = pretty n <!> pretty t

instance Pretty OpExp where
    pretty (LeafExp e) = pretty e
    pretty (Op o e1 e2) = parens $ pretty o <+> pretty e1 <+> pretty e2
instance Pretty Exp where
    pretty (VarExp n t) = parens $ pretty n <> colon <> pretty t
    pretty (ConExp n t) = parens $ pretty n <> colon <> pretty t
    pretty (LitExp v t) = parens $ pretty v <> colon <> pretty t
    pretty (InfixOpExp e t) = parens $ pretty e <> colon <> pretty t
    pretty (FExp e1 e2 t) = parens $ pretty e1 <!> pretty e2 <> colon <> pretty t
    pretty (MinusExp e t) = parens $ pretty "~" <> pretty e <> colon <> pretty t
    pretty (MinusFloatExp e t) = parens $ pretty "~." <> pretty e <> colon <> pretty t
    pretty (LambdaExp p e t) = parens $ 
                            pretty "\\" 
                                       <> pretty p <> pretty "->"
                                       <> pretty e <> colon <> pretty t
    pretty (LetExp ds e t) = parens $ pretty "let" <!> pretty ds 
                             <+> pretty "in" <!> pretty e <> colon <> pretty t
    pretty (IfExp e1 e2 e3 t) = parens $ pretty "if" <!> pretty e1 
                                <+> pretty "then" <!> pretty e2 
                                <+> pretty "else" <!> pretty e3
                                <> colon <> pretty t
    pretty (CaseExp e alts t) = parens $ pretty "case" <!> pretty e 
                                <!> pretty "of" <!> pretty alts
                                <> colon <>  pretty t
    pretty (ParensExp e t) = pretty e <> colon <> pretty t -- No parens needed here
    pretty (TupleExp e t) = parens $ pretty "#" <> pretty e <> colon <> pretty t
    pretty (ListExp e t) = parens $ pretty e <> colon <> pretty t

instance Pretty Pattern where
    pretty (VarPat n)    = pretty n
    pretty (ConPat n p) = pretty n <!> pretty p
    pretty (TuplePat t)  = pretty "#" <> pretty t
    pretty (ListPat l)   = pretty l
    pretty (HeadTailPat n1 n2) = pretty n1 <> colon <> pretty n2
    pretty (WildcardPat) = pretty '_'

instance Pretty Alternative where
    pretty (Alternative p e) = pretty p <> pretty " -> " <!> pretty e

(<!>) a b = a <> space <> (hang 4) b

