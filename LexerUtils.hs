module LexerUtils
    (
     Position
    ,showPosition    
    ,AlexInput(..)
    ,alexGetChar
    ,alexInputPrevChar
    )
    where

-- Position identifies a location in the source
type Position = (String, Int, Int) -- (File, line, col)

showPosition :: Position -> String
showPosition (file, line, col) = "line " ++ show line 
                                 ++ ", column " ++ show col 
                                 ++ ", in file " ++ file

data AlexInput =
    AlexInput 
    {
      position :: !Position
    , input :: String 
    } 
    deriving (Show, Eq)

alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar (AlexInput p (x:xs)) = Just (x, AlexInput (alexAdvance p x) xs)
alexGetChar (AlexInput _ []) = Nothing

alexInputPrevChar :: AlexInput -> Char 
alexInputPrevChar _ = error "Lexer doesn't implement alexInputPrevChar"



-- Internal functions

alexAdvance :: Position -> Char -> Position
alexAdvance (f, l, _) '\n' = (f, l + 1, 1)
alexAdvance (f, l, c) '\t' = (f, l, (c+8) `div` 8 * 8)
alexAdvance (f, l, c)  _   =  (f, l, c + 1)

