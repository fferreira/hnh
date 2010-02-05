{

module Parser
  (
   parser
  )
where

import Lexer(HasntToken(..))
import Syntax

}
  
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
   'defualt'		{ DefaultToken }
   'if'			{ IfToken }
   'then'		{ ThenToken }
   'else'		{ ElseToken }
   'in'			{ InToken }
   'infix'		{ InfixToken }
   'infixl' 		{ InfixlToken }
   'infixr'		{ InfixrToken }
   'let'		{ LetToken }
   'newtype'		{ NewtypeToken }
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
program : declarations			{ Program (reverse $1) } --TODO is reverse correct?

-- Declarations

declarations :: { [Declaration] }
declarations : declaration		{ [$1] }
	     | declarations declaration	{ $2 : $1 }

declaration :: { Declaration }
declaration : 'type' simpletype '=' type
	      	     			{ TypeDcl $2 $4 }
declaration : 'data' CONID '=' constrs	{ DataDcl $2 [] $4 } 

-- Types

simpletype :: { Name }
simpletype : CONID			{ $1 } -- TODO add polymorphic types !!

types :: { [Type] }
types : types ',' type			{ $3 : $1 }
      | type ',' type			{ [$3, $1] }

type :: { Type }
type : btype '->' type			{ FuncType $1 $3 }
     | btype 	  			{ $1 }

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
--       | atype				{ [$1] }
       | 				{ [] }  

-- Code

{
parser = hasnt

parserError :: [HasntToken] -> a
parserError tokens = error ( "Parse error:" ++ (show (head tokens)))
}
