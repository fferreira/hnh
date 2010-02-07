{
module Lexer (lexer) where

import Token
}

%wrapper "posn"

$space = \32 -- space character (TODO there must be a clean portable way to do this)

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

$white+				;
"--".*				{\p s -> (pos p, LineComment s)}

--$special			{\p s -> (pos p, SpecialChar s)}
\(				{\p _ -> (pos p, LeftParen)}
\)				{\p _ -> (pos p, RightParen)}
\,				{\p _ -> (pos p, Comma)}
\;				{\p _ -> (pos p, SemiColon)}
\[				{\p _ -> (pos p, LeftSq)}
\]				{\p _ -> (pos p, RightSq)}
\`				{\p _ -> (pos p, BackQuote)}
\{				{\p _ -> (pos p, LeftCurly)}
\}				{\p _ -> (pos p, RightCurly)}

-- Used reserved words

"case"				{\p _ -> (pos p, CaseToken)}
"data"				{\p _ -> (pos p, DataToken)}
"else"				{\p _ -> (pos p, ElseToken)}
"if"				{\p _ -> (pos p, IfToken)}
"in"				{\p _ -> (pos p, InToken)}
"infix"				{\p _ -> (pos p, InfixToken)}
"infixl"			{\p _ -> (pos p, InfixlToken)}
"infixr"			{\p _ -> (pos p, InfixrToken)}
"let"				{\p _ -> (pos p, LetToken)}
"of"				{\p _ -> (pos p, OfToken)}
"then"				{\p _ -> (pos p, ThenToken)}
"type"				{\p _ -> (pos p, TypeToken)}
"where"				{\p _ -> (pos p, WhereToken)}

-- UnusedReservedWords, are Haskell reserved words
-- currently not used in hasnt

@unUsedReservedWord		{\p s -> (pos p, UnusedReservedWord s)}

-- Reserved operators

".."				{\p _ -> (pos p, DoubleDotOp)}
":"				{\p _ -> (pos p, ColonOp)}
"::"				{\p _ -> (pos p, DoubleColonOp)}
"="				{\p _ -> (pos p, EqualsOp)}
"\\"				{\p _ -> (pos p, BackSlashOp)}
"|"				{\p _ -> (pos p, BarOp)}
"<-"				{\p _ -> (pos p, LeftArrowOp)}
"->"				{\p _ -> (pos p, RightArrowOp)}
"@"				{\p _ -> (pos p, AtOp)}
"~"				{\p _ -> (pos p, TildeOp)}
"=>"				{\p _ -> (pos p, DoubleArrowOp)}



@varid				{\p s -> (pos p, VariableName s)}
@conid				{\p s -> (pos p, ConstructorName s)}
@varsym				{\p s -> (pos p, VariableSymbol s)}
@integer			{\p s -> (pos p, IntegerLiteral (read s))}
@float				{\p s -> (pos p, FloatLiteral (read s))}
@string				{\p s -> (pos p, StringLiteral s)}
@char				{\p s -> (pos p, CharLiteral s)}

.				{\p s -> (pos p, Unexpected s)}

-- TODO add nested comments,(YES)
-- TODO check EOF handling


{

lexer :: String -> [(Position, HasntToken)]
lexer = alexScanTokens

pos :: AlexPosn -> Position
pos (AlexPn _ line col) = (line, col)

}

