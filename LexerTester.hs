module LexerTester
    (
     testLexer
    ) where

import Lexer

testPatterns :: [(String, HasntToken)]
testPatterns = [("-- comment", LineComment "-- comment"),
                ("case"  , ReservedWord "case"),
                ("class" , UnusedReservedWord "class"),
                
                ("::"    , ReservedOp "::"),                                                      

                ("var"   , VariableName "var"),
                ("Type"  , ConstructorName "Type"),
                ("+:+"   , VariableSymbol "+:+"),

                ("1"     , IntegerLiteral 1),
                ("12"    , IntegerLiteral 12),
                ("0o70"  , IntegerLiteral 56),
                ("0O70"  , IntegerLiteral 56),
                ("0xFF"  , IntegerLiteral 255),
                ("0XF"   , IntegerLiteral 15),
                ("0xf"   , IntegerLiteral 15),
                ("0Xf"   , IntegerLiteral 15),

                ("1.0"   , FloatLiteral 1),
                ("1.1"   , FloatLiteral 1.1),
                ("1E2"   , FloatLiteral 100),
                ("1e2"   , FloatLiteral 100),
                ("1.1E2" , FloatLiteral 110),
                ("1.1e2" , FloatLiteral 110),

                ("\"hola\""     , StringLiteral "\"hola\""),
                ("\"ho\nla\""   , StringLiteral "\"ho\nla\""),
                ("\"h'o\32la\"" , StringLiteral "\"h'o\32la\""),
                
                ("'hola'"      , CharLiteral "'hola'"),
                ("'ho\nla'"    , CharLiteral "'ho\nla'"),
                ("'h\"o\32la'" , CharLiteral "'h\"o\32la'")

               ]


result = map (\(s, t) -> (snd (head (lexer s)) == t) && (length (lexer s) == 1)) testPatterns

testLexer = foldl (&&) True result