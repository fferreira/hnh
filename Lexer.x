{
module Lexer (getToken) where 

import Token
--import ParserMonad
import LexerUtils

}

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

@string = $quote ($graphic_in_string | $white | @escape)* $quote
@char	= $singlequote ($graphic_in_char | $white | @escape)* $singlequote

@varid = 
       $small ($small | $large | $digit | \')*

@conid =
       $large ($small | $large | $digit | \')*

@varsym =
	$symbol ($symbol | :)*


tokens :-

$white+				;
"--".*				;

\(				{\(i, s) -> return $ (LeftParen, i)}
\)				{\(i, s) -> return $ (RightParen, i)}
\,				{\(i, s) -> return $ (Comma, i)}
\;				{\(i, s) -> return $ (SemiColon, i)}
\[				{\(i, s) -> return $ (LeftSq, i)}
\]				{\(i, s) -> return $ (RightSq, i)}
\`				{\(i, s) -> return $ (BackQuote, i)}
\{				{\(i, s) -> return $ (LeftCurly, i)}
\}				{\(i, s) -> return $ (RightCurly, i)}
\_				{\(i, s) -> return $ (Underscore, i)}

-- Reserved words

"case"				{\(i, s) -> return $ (CaseToken, i)}
"data"				{\(i, s) -> return $ (DataToken, i)}
"else"				{\(i, s) -> return $ (ElseToken, i)}
"if"				{\(i, s) -> return $ (IfToken, i)}
"in"				{\(i, s) -> return $ (InToken, i)}
"infix"				{\(i, s) -> return $ (InfixToken, i)}
"infixl"			{\(i, s) -> return $ (InfixlToken, i)}
"infixr"			{\(i, s) -> return $ (InfixrToken, i)}
"let"				{\(i, s) -> return $ (LetToken, i)}
"of"				{\(i, s) -> return $ (OfToken, i)}
"then"				{\(i, s) -> return $ (ThenToken, i)}
"type"				{\(i, s) -> return $ (TypeToken, i)}

-- Reserved operators

".."				{\(i, s) -> return $ (DoubleDotOp, i)}
":"				{\(i, s) -> return $ (ColonOp, i)}
"::"				{\(i, s) -> return $ (DoubleColonOp, i)}
"="				{\(i, s) -> return $ (EqualsOp, i)}
"\\"				{\(i, s) -> return $ (BackSlashOp, i)}
"|"				{\(i, s) -> return $ (BarOp, i)}
"->"				{\(i, s) -> return $ (RightArrowOp, i)}
"@"				{\(i, s) -> return $ (AtOp, i)}
"~"				{\(i, s) -> return $ (TildeOp, i)}

@varid				{\(i, s) -> return $ (VariableName s, i)}
@conid				{\(i, s) -> return $ (ConstructorName s, i)}
@varsym				{\(i, s) -> return $ (VariableSymbol s, i)}
@integer			{\(i, s) -> return $ (IntegerLiteral (read s), i)}
@float				{\(i, s) -> return $ (FloatLiteral (read s), i)}
@string				{\(i, s) -> return $ (StringLiteral s, i)}
@char				{\(i, s) -> return $ (CharLiteral s, i)}

.				{\(i, s) -> return $ (Unexpected s, i)}

-- TODO add nested comments

{


getToken :: AlexInput -> Position -> (HasntToken, AlexInput)
getToken = \i ->
      do case alexScan i 0 of
          AlexEOF -> return (EOFToken, i)
          AlexError _ -> fail $ "Lexical error at " ++ (showPosition (position i))
          AlexSkip i' _ -> getToken i'
          AlexToken i' l a -> a (i', take l (input i))

}

