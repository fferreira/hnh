{
module Lexer (lexer, getToken) where  --TODO remove the comments and export the right thing

import Token
import ParserMonad

}

$space = \32 -- space character

$digit = [0-9]
$octit = [0-7]
$hexit = [0-9A-Fa-f]

$small   =     [a-z]
$large   =     [A-Z]
$symbol  =     [!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~] -- from haskell report !#$%&*+./<=>?@\^|-~
$special =     [\(\(\,\;\[\]\`\{\}] -- from haskell report (),;[]`{}
$graphic =     [$small $large $symbol $digit $special \: \" \']
$charesc =     [a b f n r t v \\ \" \' \&]

$graphic_in_string  = [$graphic # [\" \\]]
$graphic_in_char    = [$graphic # [\' \\]]

$quote		    = \"
$singlequote	    = \'



-- Reserved words as per Haskell '98 std, but several will be removed
-- because we won't be needing them (probably it will be a good idea to
-- have them anyways for compatibility. (Maybe separate a used and an 
-- unused category). `

-- decide the status of infix|infixl|infixr

@integer =
	 $digit+ |
	 0o$octit+ | 0O$octit+ |
	 0x$hexit+ | 0X$hexit+

@exponent = (e | E)(\+ | \-)? $digit+
@float =
       $digit+ \. $digit+ |
       $digit+ \.? $digit* @exponent

@escape =
	\\ ($charesc | $digit+ | (o$octit+) | (x$hexit+))

-- $space and $white are redundant, but
-- but were added for "compatibility"
-- with the Haskell 98 report

@string = $quote ($graphic_in_string | $space | $white | @escape)* $quote
@char	= $singlequote ($graphic_in_char | $space | $white | @escape)* $singlequote

@usedReservedWord =
	case | data | else | if |
	in | infix | infixl | infixr | let
	of | then | type | where

@unUsedReservedWord =
	do | as | hiding | class | deriving | import |
	instance | module | qualified | default | newtype

@reservedWord =
	@usedReservedWord | @unUsedReservedWord

@reservedOp =
	\.\. | :: | : | = | \| | \<\- | \-\> | @ | \~ | \=\>

@varid = 
       $small ($small | $large | $digit | \')*

@conid =
       $large ($small | $large | $digit | \')*

@varsym =
	$symbol ($symbol | :)*


tokens :-

\n				{\s -> return SkipNewLine  }
$white+				;
"--".*				{\s -> mkT (LineComment s) } --TODO avoid lambda mkt . LineComment

\(				{\s -> mkT LeftParen }
\)				{\s -> mkT RightParen }
\,				{\s -> mkT Comma }
\;				{\s -> mkT SemiColon }
\[				{\s -> mkT LeftSq }
\]				{\s -> mkT RightSq }
\`				{\s -> mkT BackQuote }
\{				{\s -> mkT LeftCurly }
\}				{\s -> mkT RightCurly }

-- Used reserved words

"case"				{\s -> mkT CaseToken }
"data"				{\s -> mkT DataToken }
"else"				{\s -> mkT ElseToken }
"if"				{\s -> mkT IfToken }
"in"				{\s -> mkT InToken }
"infix"				{\s -> mkT InfixToken }
"infixl"			{\s -> mkT InfixlToken }
"infixr"			{\s -> mkT InfixrToken }
"let"				{\s -> mkT LetToken }
"of"				{\s -> mkT OfToken }
"then"				{\s -> mkT ThenToken }
"type"				{\s -> mkT TypeToken }
"where"				{\s -> mkT WhereToken }

-- UnusedReservedWords, are Haskell reserved words
-- currently not used in hasnt

@unUsedReservedWord		{ mkT . UnusedReservedWord }

-- Reserved operators

".."				{\s -> mkT DoubleDotOp }
":"				{\s -> mkT ColonOp }
"::"				{\s -> mkT DoubleColonOp }
"="				{\s -> mkT EqualsOp }
"\\"				{\s -> mkT BackSlashOp }
"|"				{\s -> mkT BarOp }
"<-"				{\s -> mkT LeftArrowOp }
"->"				{\s -> mkT RightArrowOp }
"@"				{\s -> mkT AtOp }
"~"				{\s -> mkT TildeOp }
"=>"				{\s -> mkT DoubleArrowOp }

@varid				{\s -> mkT $ VariableName s }
@conid				{\s -> mkT $ ConstructorName s }
@varsym				{\s -> mkT $ VariableSymbol s }
@integer			{\s -> mkT $ IntegerLiteral (read s) }
@float				{\s -> mkT $ FloatLiteral (read s) }
@string				{\s -> mkT $ StringLiteral s }
@char				{\s -> mkT $ CharLiteral s }

.				{\s -> mkT $ Unexpected s }

-- TODO add nested comments

{

lexer :: (HasntToken -> ParserM a) -> ParserM a
lexer = runL $ do getToken

getToken :: LexerM a HasntToken
getToken = do i <- getInput
	      case alexScan i 0 of
	      	   AlexEOF -> return EOFToken
		   AlexError e -> fail $ "Lexical error at " ++ (show e)	
		   	       	       ++ "||" ++ showPosition (position i)
		   AlexSkip i' l -> (skip l) >> getToken
		   AlexToken i' l a -> let 
		   	     	       	 --a' :: String -> LexerM (LexerA HasntToken) (LexerA HasntToken)
					 -- ^ ????
                                         a' = a
		   	     	       in case (a' (takeInput l i) (i', takeInput l i)) of
                                           RegularAction act -> (skip l) >> (return $ act)
					   SkipNewLine -> {-skipNewLine >>-} getToken
                                           otherwise -> error "agregar lo que falta"

takeInput :: Int -> AlexInput -> String
takeInput l (AlexInput p s) = take l s --AlexInput p (take l s)

}

