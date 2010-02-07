{

module Parser
  (
   parser
  )
where

import Token
import Syntax
import ParserMonad

}
 
%monad { P }  
%name hasnt
%tokentype { HasntToken }
%error { parserError }

%token
	comment		{ LineComment $$ }

-- Special Characters

   '('			{ LeftParen }
   ')'			{ RightParen }
   ','			{ Comma }
   ';'			{ SemiColon }
   '['			{ LeftSq }
   ']'			{ RightSq }
   '`'			{ BackQuote }
   '{'			{ LeftCurly }
   '}'			{ RightCurly }

-- Reserved Words

   'case'		{ CaseToken }
   'data'		{ DataToken }
   'if'			{ IfToken }
   'then'		{ ThenToken }
   'else'		{ ElseToken }
   'in'			{ InToken }
   'infix'		{ InfixToken }
   'infixl' 		{ InfixlToken }
   'infixr'		{ InfixrToken }
   'let'		{ LetToken }
   'of'			{ OfToken }
   'type'		{ TypeToken }
   'where'		{ WhereToken }

-- Unused Reserved Word

   UNUSEDWORD		{ UnusedReservedWord $$ }

-- Reserved operands

   '..'			{ DoubleDotOp }
   ':'			{ ColonOp }
   '::'			{ DoubleColonOp }
   '='			{ EqualsOp }
   '\\'			{ BackSlashOp }
   '|'			{ BarOp }
   '<-'			{ LeftArrowOp }
   '->'			{ RightArrowOp }
   '@'			{ AtOp }
   '~'			{ TildeOp }
   '=>'			{ DoubleArrowOp }

-- Variable and Constructor Names

   VARID    		{ VariableName $$ }
   CONID		{ ConstructorName $$ }
   VARSYM		{ VariableSymbol $$ }

-- Literals

   INT			{ IntegerLiteral $$ }
   FLOAT		{ FloatLiteral $$ }
   STRING		{ StringLiteral $$ }
   CHAR			{ CharLiteral $$ }

-- Precedences
   
-- TODO fill preferences

%% -- Rules

-- Program

program :: { Program }
program : topdecls			{ Program (reverse $1) }

-- Declarations

topdecls :: { [Declaration] }
topdecls : topdecl		{ [$1] }
	     | topdecls topdecl	{ $2 : $1 }

topdecl :: { Declaration }
topdecl : 'type' simpletype '=' type	  { TypeDcl $2 $4 }
topdecl : 'data' CONID '=' constrs	  { DataDcl $2 [] $4 }    -- TODO fill the empty list param

topdecl : decl	    	      		  { $1 }


-- Non-type declarations

decl :: { Declaration }
delc : gendecl				{ $1 }

gendecl :: { Declaration }
gendecl : typeSigDecl			{ $1 }
	| fixityDecl			{ $1 }
--	| valdef			{ $1 }

typeSigDecl :: { Declaration}
typeSigDecl : vars '::' type		{ TypeSigDcl $1 $3 }

vars :: { [Name] }
vars : vars ',' var			{ $3 : $1 }
     | var  				{ [$1] } -- TODO que joraca pongo aca ?

var :: { Name }
var : VARID				{ $1 }
    | '(' VARSYM ')'			{ $2 }

fixityDecl :: { Declaration }
fixityDecl : assocFix precedence ops	{ FixityDcl $1 $2 $3 }

assocFix :: { Associativity }
assocFix : 'infix'			{ NonAssoc }
	 | 'infixl'			{ LeftAssoc }
	 | 'infixr'			{ RightAssoc }

precedence :: { Int }
precedence : INT			{% if $1 < 0 || $1 > 9 
	     				      then returnError "Wrong fixity specified"
					      else returnOk $1
	     				} -- Should be 0-9
	   | 				{ 9 } -- default precedence

ops :: { [Operator] }
ops : ops ',' op			{ $3 : $1 }
    | op      				{ [$1] }

op :: { Operator }
op : VARSYM				{ $1 }
   | '`' VARID '`'			{ $2 }
{-
valdef :: { Declaration }
valdef : exp0b rhs {- optwhere -}	{ }
-}
-- Types

simpletype :: { Name }
simpletype : CONID			{ $1 } -- TODO add polymorphic types !!

types :: { [Type] }
types : types ',' type			{ $3 : $1 }
      | type ',' type			{ [$3, $1] }

type :: { Type }
type : btype '->' type			{ FuncType $1 $3 }
     | btype 	  			{ $1 }

{-
Two shift reduce conflicts when parsing btype atpye
when finding a ( the parser can:

* shift the ( and continue with the a type CORRECT!
* reduce the btype before the ( WRONG!

One conflict is introduced when type -> btype . '->' type
The other one is introduced when atypes -> atypes . atype
-}
btype :: { Type }
btype : btype atype			{ AppType $1 $2 }
      | atype 				{ $1 }

atype :: { Type }
atype : CONID				{ ConsType $1 } -- tyvar in the report (TODO something mising?)
      | '(' types ')'			{ TupleType $2 } -- TODO Reverse $2 ??
      | '[' type ']'			{ $2 } -- TODO COMPLETE List type
      | '(' type ')'			{ $2 } -- One uple does not exist

-- 'data' declarations

constrs :: { [ConstructorDeclaration] }
constrs : constrs '|' constr		{ $3 : $1 }
	| constr			{ [$1] }

constr :: { ConstructorDeclaration }
constr :  CONID atypes			{ ConsDcl $1 $2 }

atypes :: { [Type] }
atypes : atypes atype			{ $2 : $1 }
       | 				{ [] }  

-- Code

{
parser = hasnt

-- TODO use the location information for the error report
parserError :: [HasntToken] -> P a
parserError (token:_) = returnError ("Parse Error" ++ (show token))

}
