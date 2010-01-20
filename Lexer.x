{
module Lexer(HasntToken(..), lexer) where
}

%wrapper "basic" -- change to later use "posn"

$digit = [0-9]
$octit = [0-7]
$hexit = [0-9A-Fa-f]

$small   =     [a-z]
$large   =     [A-Z]
$symbol  =     [!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~] -- from haskell report !#$%&*+./<=>?@\^|-~
$special =     [\(\(\,\;\[\]\'\{\}] -- from haskell report (),;[]'{}


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

@graphic =
	 $small | $large | $symbol |$digit | $special

@usedReservedWord =
	as | case | data | default | do | else | hiding | if |
	in | infix | infixl | infixr | let | newtype |
	of | qualified | then | type | where

@unUsedReservedWord =
	class | deriving | import | instance | module

@reservedWord =
	@usedReservedWord | @unUsedReservedWord 

@varid = 
       $small ($small | $large | $digit | \')*

@conid =
       $large ($small | $large | $digit | \')*


tokens :-

$white+				;
"--".*				{\s -> LineComment s}
--$symbol+			{\s -> Symbol s}
@reservedWord			{\s -> ReserverdWord s}
@varid				{\s -> VariableName s}
@conid				{\s -> ConstructorName s}
@integer			{\s -> IntegerLiteral (read s)}
@float				{\s -> FloatLiteral (read s)}


{
data HasntToken =
     LineComment String		|
     ReserverdWord String	|
     VariableName String	|
     ConstructorName String	|
     IntegerLiteral Integer	|
     FloatLiteral Double	|
     Symbol String
     deriving (Eq, Show)

lexer :: String -> [HasntToken]
lexer = alexScanTokens

}

