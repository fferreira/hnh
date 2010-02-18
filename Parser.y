{

module Parser
  (
   parser
  )
where

import Token
import Syntax
import ParserMonad(ParserM, returnError, returnOk, lexer)
import ParserUtils(addType, litToExp, assembleInfixOperator, checkPat)

}
 
%monad { ParserM }
%lexer { lexer } { EOFToken }  
%name parser
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

-- Reserved operands

   '..'			{ DoubleDotOp }
   ':'			{ ColonOp }
   '::'			{ DoubleColonOp }
   '='			{ EqualsOp }
   '\\'			{ BackSlashOp }
   '|'			{ BarOp }
   '->'			{ RightArrowOp }
   '@'			{ AtOp }
   '~'			{ TildeOp }

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
topdecls : topdecl optsc		  { [$1] }
	 | topdecls topdecl optsc	  { $2 : $1 }

topdecl :: { Declaration }
topdecl : 'type' simpletype '=' type	  { TypeDcl (fst $2) (snd $2) $4 }
topdecl : 'data' simpletype '=' constrs	  { DataDcl (fst $2) (snd $2) (reverse $4) }

topdecl : decl	    	      		  { $1 }


-- Non-type declarations

decl :: { Declaration }
decl : gendecl				{ $1 }
     | VARID apats rhs			{ FunBindDcl $1 (reverse $2) $3 } --funlhs
     | pat rhs				{ PatBindDcl $1 $2 }

decls :: { [Declaration] }  -- one or more declarations
decls : decls optsc decl		{ $3 : $1 }
      | decl optsc  			{ [$1] }

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
fixityDecl : assocFix precedence ops	{ FixityDcl $1 $2 (reverse $3) }

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

-- Right Hand Side

rhs :: { Rhs }
rhs : '=' exp optsc			{ UnGuardedRhs $2 }
    | gdrhss				{ GuardedRhs (reverse $1) }

gdrhss :: { [Guard] }
gdrhss : gdrhss gdrhs			{ $2 : $1 }
       | gdrhs				{ [$1] }

gdrhs :: { Guard }
gdrhs : gd '=' exp optsc		{ Guard $1 $3 }  -- TODO add type bool to the first

gd :: { Expr }
gd : '|' exp				{ $2 }

-- Expressions

exp :: { Expr }
exp : expi '::' type 			{ addType $1 $3 } 
    | expi 				{ $1 }

expi :: { Expr }
expi : expi op expi			{ assembleInfixOperator $1 $2 $3 }
     | '~' expi				{ MinusExp $2 ut }
     | exp10   				{ $1 }

exp10 :: { Expr }
exp10 :	fexp				{ $1 }
      | '\\' apats '->' exp		{ LambdaExp $2 $4 ut }
      | 'let' lcb decls rcb 'in' exp    { LetExp (reverse $3) $6 ut }
      | 'if' exp 'then' exp 'else' exp  { IfExp $2 $4 $6 ut }
      | 'case' exp 'of' lcb alts rcb   	{ CaseExp $2 (reverse $5) ut }

alts :: { [Alternative] }
alts : alts alt				{ $2 : $1 }
     | alt  			       	{ [$1] }

alt :: { Alternative }
alt : pat '->' exp ';'			{ Alternative $1 $3 }

fexp :: { Expr }
fexp : fexp aexp			{ FExp $1 $2 ut }
     | aexp 				{ $1 }

aexp :: { Expr }
aexp : VARID				{ VarExp $1 ut }
     | CONID				{ ConExp $1 ut }
     | literal				{ litToExp $1 }
     | '(' exp ')'			{ ParensExp $2 ut }
     | '(' tupleexps ')'		{ TupleExp (reverse $2) ut } -- TODO add (ut, ut..) as type?
     | '[' listexps ']'			{ ListExp (reverse $2) ut }

tupleexps :: { [Expr] }
tupleexps : tupleexps ',' exp		{ $3 : $1 }
	  | exp ',' exp	  		{ [$3, $1] }

listexps :: { [Expr] }
listexps : listexps ',' exp		{ $3 : $1 }
	 | exp	    			{ [$1] }

-- Patterns

pats :: { [Pattern] } -- zero or more patterns
pats : pats pat				{ $2 : $1 }
     | 	    				{ [] }

-- checkPat is checked redundantly -- TODO improve this
pat :: { Pattern }
pat : VARID				{ VarPat $1 }
     | VARID '@' pat			{% checkPat (AsPat $1 $3) }
     | VARID ':' VARID			{% checkPat (HeadTailPat $1 $3) } 
     | CONID pats			{% checkPat (ConPat $1 $2) }
     | literal				{ LitPat $1 }
     | '_'				{% checkPat (WildcardPat) }
     | '(' pat ')'			{% checkPat ($2) }
     | '(' tuplepats ')'		{% checkPat (TuplePat (reverse $2)) }  
       	   	     			  	   -- ^ it has to be >= 2 to be a tuple
     | '[' listpats ']'			{% checkPat (ListPat (reverse $2)) }

apats :: { [Pattern] } -- one or more patterns
apats : apats pat			{ $2 : $1 }
      | pat  				{ [$1] }

tuplepats :: { [Pattern] }  -- two or more
tuplepats : tuplepats ',' pat		{ $3 : $1 }
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

literal :: { LiteralValue }
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

-- curly brace handling

lcb :: { () } -- optional curly braces
lcb : '{'				{ () }
    |  					{ () }

rcb :: { () } -- optional curly braches
rcb : '}'				{ () }
    | 					  { () }

{

ut :: Type
ut = UnknownType

parserError :: HasntToken -> ParserM a
parserError token = returnError ("Parse Error:" ++ (show token))

}
