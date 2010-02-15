{

module Parser
  (
   parser
  )
where

import Token
import Syntax
import ParserMonad
import Lexer(lexer)


}
 
%monad { ParserM }
%lexer { lexer } { EOFToken }  
%name hasnt
%tokentype { HasntToken }
%error { parserError }

%token
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
   '_'			{ Underscore }

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

-- Debugging tokens
 'joker'		{ Joker }

-- Precedences
   
-- TODO fill preferences

%% -- Rules


-- Program

program :: { Program }
program : topdecls			{ Program (reverse $1) }

-- Declarations

topdecls :: { [Declaration] }
topdecls : topdecl optsc	{ [$1] }
	 | topdecls topdecl	{ $2 : $1 }

topdecl :: { Declaration }
topdecl : 'type' simpletype '=' type	  { TypeDcl (fst $2) (snd $2) $4 }
topdecl : 'data' simpletype '=' constrs	  { DataDcl (fst $2) (snd $2) $4 }

topdecl : decl	    	      		  { $1 }


-- Non-type declarations

decl :: { Declaration }
decl : gendecl				{ $1 }
     | VARID aPats rhs			{ FunBindDcl $1 (reverse $2) $3 } --funlhs
     | pat rhs				{ PatBind $1 $2 }

gendecl :: { Declaration }
gendecl : typeSigDecl			{ $1 }
	| fixityDecl			{ $1 }

typeSigDecl :: { Declaration}
typeSigDecl : vars '::' type		{ TypeSigDcl $1 $3 }

vars :: { [Name] }
vars : vars ',' var			{ $3 : $1 }
     | var  				{ [$1] } 

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
   | ':'       				{ ":" } -- TODO are there other operators missing?

-- Expressions

rhs :: { Rhs }
rhs :  '=' 'joker'			{ UnGuardedRhs $ VarExp "joker" }

{-
exp0b :: { Expr }
exp0b :	'joker'				{ VarExp "joker" }
-}

-- Patterns

pat :: { Pattern }
pat : pat0				{ $1 }

pats :: { [Pattern] }
pats : pats pat				{ $2 : $1 }
     | 	    				{ [] }

pat0 :: { Pattern } -- TODO isn't really a pati?
pat0 : pat10				{ $1 }
     | pat0 op pat0			{ InfixPat $1 $2 $3 }

pat10 :: { Pattern }
pat10 : aPat				{ $1 }
      | CONID pats			{ConsPat $1 $2}


aPat :: { Pattern }
aPat : VARID				{ VarPat $1 }
     | VARID '@' aPat			{ AsPat $1 $3 } 
     | literal				{ LitPat $1 }
     | '_'				{ WildcardPat }
     | '(' pat ')'			{ $2 }
     | '(' tuplepats ')'		{ TuplePat (reverse $2) }  -- it has to be >= 2 to be a tuple
     | '[' listpats ']'			{ ListPat (reverse $2) }

aPats :: { [Pattern] }
aPats : aPats aPat			{ $2 : $1 }
      | aPat  				{ [$1] }

tuplepats :: { [Pattern] }  -- two or more
tuplepats : tuplepats pat		{ $2 : $1 }
	  | pat ',' pat			{ [$3, $1] }

listpats :: { [Pattern] }  -- one or more
listpats: listpats ',' pat		{ $3 : $1 } 
	| pat	   			{ [$1] }  	   

-- Types

simpletype :: { (Name, [Name]) }
simpletype : CONID tyvars		{ ($1, reverse $2) } -- TODO add polymorphic types !!

types :: { [Type] }
types : types ',' type			{ $3 : $1 }
      | type ',' type			{ [$3, $1] }

type :: { Type }
type : btype '->' type			{ FuncType $1 $3 }
     | btype 	  			{ $1 }

tyvars :: { [Name] }
tyvars : tyvars tyvar			{ $2 : $1 }
       | 				{ [] }

tyvar :: { Name }
tyvar : VARID				{ $1 }

-- TODO see shift/reduce conflicts that seem to be introduced around 

btype :: { Type }
btype : btype atype			{ AppType $1 $2 }
      | atype 				{ $1 }

atype :: { Type }
atype : CONID				{ ConsType $1 } 
      | VARID				{ VarType $1 }
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

-- literal handling

literal :: { LiteralValue}
literal : INT				{ LiteralInt $1 }
	| FLOAT				{ LiteralFloat $1 }
	| STRING			{ LiteralString $1 }
	| CHAR				{ LiteralChar $1 }

-- semicolon handling

optsc :: { () }  -- optional semicolons
      : semicolons			{ () }
      |					{ () }

semicolons :: { () }
	   : optsc ';'			{ () }

{
parser = hasnt

parserError :: HasntToken -> ParserM a
parserError token = returnError ("Parse Error:" ++ (show token))

}
