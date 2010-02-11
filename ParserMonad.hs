module ParserMonad 
    (
      Position
    , showPosition
    , LayoutContext(..)
    , ParseState
    , ParseResult(..)
    , ParserM(..)
    , runParser , runParserWithFileName
    , returnOk , returnError
    , AlexInput(..)
    , alexGetChar
    , alexInputPrevChar
    )
    where

import Token

-- Position identifies a location in the source
type Position = (String, Int, Int) -- (File, line, col)

showPosition :: Position -> String
showPosition (file, line, col) = "line " ++ show line 
                                 ++ ", column " ++ show col 
                                 ++ ", in file " ++ file

-- default input stream (aka fileName) name
defaultName :: String
defaultName = "input stream"

data LayoutContext = NoLayout 
                   | LayoutLevel Int
                     deriving (Show, Eq) -- TODO Ord too?

type ParseState = [LayoutContext]

data ParseResult a = Ok ParseState a 
                   | Failed Position String -- TODO add ParseState to error message?
                     deriving Show

--  Monad for parsing
newtype ParserM a = ParserM { runP ::
                              AlexInput
                              -- v-- are we using this as we should ??
                              -> Position   -- Location of the last token read
                              -> ParseState -- layout info
                              -> ParseResult a
                            }

runParserWithFileName :: String -> ParserM a -> String -> ParseResult a
runParserWithFileName fileName (ParserM parse) program = parse (AlexInput start program) start2 []
    where
      start = (fileName, 1, 0)
      start2 =("none", -1, -1) 

runParser :: ParserM a -> String -> ParseResult a
runParser = runParserWithFileName defaultName

returnOk :: a -> ParserM a
returnOk a = ParserM $ \_ _ state -> Ok state a

returnError :: String -> ParserM a
returnError msg = ParserM $ \input _ _ -> Failed (position input) msg

instance Monad ParserM where
    return a = ParserM $ \_ _ {-input lastPos-} state -> Ok state a 
    ParserM m >>= k = ParserM $ \input lastPos state -> 
                case m input lastPos state of
                  Failed pos msg -> Failed pos msg
                  Ok state' a -> runP (k a) input lastPos state' 
                                 -- ^ continue the call 'chain'
    fail msg = ParserM $ \input _ _ -> Failed (position input) msg

-- Lexer Part

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

alexInputPrevChar :: AlexInput -> Char -- TODO do we need this ?
alexInputPrevChar _ = error "Lexer doesn't implement alexInputPrevChar"

-- Internal functions

alexAdvance :: Position -> Char -> Position
alexAdvance (f, l, _) '\n' = (f, l + 1, 1)
alexAdvance (f, l, c) '\t' = (f, l, (c+8) `div` 8 * 8)
alexAdvance (f, l, c)  _   =  (f, l, c + 1)

