{
module Lexer(HasntToken(..), lexer) where
}

%wrapper "basic" -- change to later use "posn"

$space = \32 -- space character (TODO there must be a clean portable way to do this)

$digit = [0-9]
$octit = [0-7]
$hexit = [0-9A-Fa-f]

$small   =     [a-z]
$large   =     [A-Z]
$symbol  =     [!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~] -- from haskell report !#$%&*+./<=>?@\^|-~
$special =     [\(\(\,\;\[\]\'\{\}] -- from haskell report (),;[]'{}
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
	as | case | data | default | do | else | hiding | if |
	in | infix | infixl | infixr | let | newtype |
	of | qualified | then | type | where

@unUsedReservedWord =
	class | deriving | import | instance | module

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
"--".*				{\s -> LineComment s}
--$symbol+			{\s -> Symbol s}
@usedReservedWord		{\s -> ReservedWord s}
@unUsedReservedWord		{\s -> UnusedReservedWord s}
@reservedOp			{\s -> ReservedOp s}
@varid				{\s -> VariableName s}
@conid				{\s -> ConstructorName s}
@varsym				{\s -> VariableSymbol s}
@integer			{\s -> IntegerLiteral (read s)}
@float				{\s -> FloatLiteral (read s)}
@string				{\s -> StringLiteral s}
@char				{\s -> CharLiteral s}



{
data HasntToken =
     LineComment String		|
     ReservedWord String	|
     UnusedReservedWord String	|
     ReservedOp String		|
     VariableName String	|
     ConstructorName String	|
     VariableSymbol String	|
     IntegerLiteral Integer	|
     FloatLiteral Double	|
     StringLiteral String	|
     CharLiteral   String	| -- TODO should change to Char ?
     Symbol String
     deriving (Eq, Show)

lexer :: String -> [HasntToken]
lexer = alexScanTokens

}

