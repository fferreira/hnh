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
    ,Expr(..)
    ,Pattern(..)
    ,Alternative(..)
    ,GuardedAlternatives(..)
    ,GuardedAlternative(..)
    )
    where

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
    | NewTypeDcl Name [Name] ConstructorDeclaration
    | DefaultDcl [Type] -- TODO will this be implemented ?
    | TypeSigDcl [Name] Type -- TODO ??? how will this be used?
    | FixityDcl Associativity Precedence [Operator]
    | FunBindDcl Name [Pattern] Rhs
    | PatBind Pattern Rhs {-where [Declaration] -}
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
    | AppType Type Type -- ??
    | VarType Name      -- for parametric types
    | ConsType Name     -- the constructor of the type
    | UnknownType
      deriving(Show, Eq)

data ConstructorDeclaration -- No support for named field types
    = ConsDcl Name [Type]
      deriving(Show, Eq)

--- Expressions & Patterns

data Expr
    = VarExp Name Type
    | ConExp Name Type
    | LitExp LiteralValue Type
    | InfixOpExp Expr Operator Expr Type -- the use of an operator as a function
    | FExp Expr Expr Type -- function application expression          
    | MinusExp Expr Type
    | LambdaExp [Pattern] Expr Type
    | LetExp [Declaration] Expr Type
    | IfExp Expr Expr Expr Type
    | CaseExp Expr [Alternative] Type
    | ParensExp Expr Type
    | TupleExp [Expr] Type -- a tuple of expresions
    | ListExp [Expr] Type  -- a list of expresions
    | LeftSectionExp Expr Operator Type  -- TODO check if doing an in-fix expr datatype
    | RightSectionExp Operator Expr Type
    | ArithSeqExp Expr (Maybe Expr) Expr Type -- from, increment, to (no infinite sequences)
      deriving (Show, Eq)

data Pattern
    = VarPat Name
    | AsPat Name Pattern
    | ConsPat Name [Pattern] -- a type constructor 
    | LitPat LiteralValue
--    | NegPat Pattern  -- TODO does this exist? irrefutable pattern?
    | ListPat [Pattern]
    | TuplePat [Pattern]
    | ParenPat (Pattern)  -- TODO is this support important? (MAYBE?)
    | InfixPat Pattern Operator Pattern --TODO which operators shall be supported?
    | WildcardPat
      deriving (Show, Eq)

data Alternative
    = Alternative Pattern GuardedAlternatives [Declaration] 
      deriving (Show, Eq)

data GuardedAlternatives
    = GuardedAlternatives [GuardedAlternative]
    | UnGuardedAlternative Expr
      deriving (Show, Eq)

data GuardedAlternative
    = GuardedAlternative Expr Expr
      deriving (Show, Eq)
