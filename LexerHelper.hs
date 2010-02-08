module LexerHelper
    (
     AlexInput(..)
    ,alexGetChar
    ,alexInputPrevChar
    )
    where

import ParserMonad
import Token



data AlexInput = AlexInput {position :: !Position, input :: String}

alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar (AlexInput p (x:xs)) = Just (x, AlexInput (alexAdvance p x) xs)
alexGetChar (AlexInput _ []) = Nothing

alexInputPrevChar :: AlexInput -> Char -- TODO do we need this ?
alexInputPrevChar _ = error "Lexer doesn't implement alexInputPrevChar"

-- Internal functions

alexAdvance :: Position -> Char -> Position
alexAdvance (f, l, _) '\n' = (f, l + 1, 1)
alexAdvance (f, l, c) '\t' = (f, l, (c+8) `div` 8 * 8)
alexAdvance (f, l, c)  _   =  (f, l, c + 1)
