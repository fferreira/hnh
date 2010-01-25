{

module Parser
  (
   parser
  )
where

import Lexer(HasntToken(..))

}
  
%name hasnt
%tokentype { HasntToken }
%error { parserError }

%token
	comment		{ LineComment $$ }
	special		{ SpecialChar $$ }

%%

-- Rules

Foo :: { Int }
Foo : comment	{ 0 }

-- Code

{
  parser = hasnt

  parserError :: [HasntToken] -> a
  parserError _ = error "Parse error"
}
