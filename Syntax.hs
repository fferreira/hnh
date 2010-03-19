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
    along with Foobar.  If not, see <http://www.gnu.org/licenses/>.
    
    Copyright 2010 Francisco Ferreira
-}

module Syntax
    (
     Program(..)
    ,Declaration(..)
    ,Name
    ,Operator
    ,LiteralValue(..)
    ,Precedence
    ,Associativity(..)
    ,Type(..)
    ,Constructor(..)
    ,OpExp(..)
    ,Exp(..)
    ,Pattern(..)
    ,Alternative(..)
    ,Identifier(..)
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
    | DataDcl Type{-Name [Name]-} [Constructor]  -- has to be a DataType
    | TypeSigDcl [Name] Type 
    | FixityDcl Associativity Precedence [Operator]
    | FunBindDcl Name [Pattern] Exp
    | PatBindDcl Pattern Exp
      deriving(Show, Eq)

type Precedence  = Int

data Associativity
    = NonAssoc
    | LeftAssoc
    | RightAssoc
      deriving(Show, Eq)

data Type -- for type declarations
    = FunType Type Type
    | TupleType [Type]
    | VarType Name      -- a polymorphic type parameter
    | ConType Name [Type]     -- the constructor of the type and its polymorphic params
    | DataType Name [Type] -- the datatype declaration
    | MetaType Int -- a meta variable for the unification process
    | UnknownType
      deriving(Show, Eq)

data Constructor 
    = ConDcl Name [Type]
    | IdConDcl Identifier [Type]
    deriving(Show, Eq)

--- Expressions & Patterns

data Identifier
     = Id Name Int
     deriving (Show, Eq)

data OpExp
    = LeafExp Exp
    | Op Operator OpExp OpExp
      deriving (Show, Eq)

data Exp 
    = VarExp Name Type
    | ConExp Name Type
    | LitExp LiteralValue Type --TODO should this be just a value?
    | InfixOpExp OpExp Type
    | FExp Exp Exp Type -- function application expression          
    | MinusExp Exp Type
    | MinusFloatExp Exp Type
    | LambdaExp [Pattern] Exp Type
    | LetExp [Declaration] Exp Type
    | IfExp Exp Exp Exp Type
    | CaseExp [Exp] [Alternative] Type
    | ParensExp Exp Type
    | TupleExp [Exp] Type -- a tuple of expresions
    | ListExp [Exp] Type  -- a list of expresions
    | IdVarExp Identifier Type -- an identifier, translated from a VarExp
    | IdConExp Identifier Type
      deriving (Show, Eq)

data Pattern
    = VarPat Name Type
    | ConPat Name [Name] Type -- a type constructor 
    | TuplePat [Name] Type
    
    | WildcardPat Type
      
    | IdVarPat Identifier Type
    | IdConPat Name [Identifier] [Type] Type
    | IdTuplePat [Identifier] Type
      deriving (Show, Eq)

data Alternative
    = Alternative [Pattern] Exp
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
  pretty (DataDcl t ts) = pretty "data " <> pretty t <> equals <!> pretty ts
  pretty (TypeSigDcl ns t) = pretty ns <> colon <> colon <> pretty t
  pretty (FixityDcl NonAssoc p ops) = pretty "infix"<+> pretty p <+> pretty ops
  pretty (FixityDcl LeftAssoc p ops) = pretty "infixl"<+> pretty p <+> pretty ops
  pretty (FixityDcl RightAssoc p ops) = pretty "infixr"<+> pretty p <+> pretty ops
  pretty (FunBindDcl n ps r) = pretty n 
                               <> pretty ps <> equals <> pretty r
  pretty (PatBindDcl p r) = pretty p <> equals <> pretty r

instance Pretty Type where
    pretty (FunType t1 t2) = parens $ pretty t1 <> pretty "->" <> pretty t2
    pretty (TupleType t) = pretty t
    pretty (VarType n) = pretty n
    pretty (ConType n []) = pretty n
    pretty (DataType t []) = pretty t
    pretty (ConType n params) = pretty n <> pretty "_"<> pretty params
    pretty (DataType t params) = pretty t <> pretty"_"<> pretty params
    pretty (UnknownType) = pretty "?"
    pretty (MetaType i) = pretty "%" <> pretty i

instance Pretty Constructor where
    pretty (ConDcl n t) = pretty n <!> pretty t
    pretty (IdConDcl i t) = pretty i <!> pretty t

instance Pretty Identifier where
  pretty (Id n num) = pretty n <> pretty "_" <> pretty num

instance Pretty OpExp where
    pretty (LeafExp e) = pretty e
    pretty (Op o e1 e2) = parens $ pretty o <+> pretty e1 <+> pretty e2
    
instance Pretty Exp where
    pretty (VarExp n t) = parens $ pretty n <> colon <> pretty t
    pretty (IdVarExp i t) = parens $ pretty i <> colon <> pretty t
    pretty (ConExp n t) = parens $ pretty n <> colon <> pretty t
    pretty (IdConExp i t) = parens $ pretty i <> colon <> pretty t
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
    pretty (VarPat n t)   = pretty n <> colon <> pretty t
    pretty (ConPat n p t) = pretty n <!> pretty p <> colon <> pretty t
    pretty (TuplePat tuple t) = pretty "#" <> pretty tuple <> colon <> pretty t
    
    pretty (WildcardPat t) = pretty '_' <> colon <> pretty t
    
    pretty (IdVarPat i t) = pretty i <> colon <> pretty t
    pretty (IdConPat n p ts t) = pretty n <!> pretty p 
                                 <> (parens $ pretty ts) 
                                 <> colon <> pretty t
    
    pretty (IdTuplePat tuple t) = pretty "#" <> pretty tuple 
                                     <> colon <> pretty t

instance Pretty Alternative where
    pretty (Alternative p e) = pretty p <> pretty " -> " <!> pretty e

(<!>) a b = a <> space <> (hang 4) b

