module ParserMonad 
    (
      Position
    , showPosition
    , LayoutContext(..)
    , ParseState
    , ParseResult(..)
    , ParserM(..)     -- TODO check if this should be exported
    , runParser , runParserWithFileName
    , returnOk , returnError
    , LexerM(..)      -- TODO check if this should be exported
    , getInput
    , skip
--    , skipNewLine
--    , skipTab
    , AlexInput(..)
--    , LexerState(..)
--    , StartCode -- TODO is this really used?
    , mkT
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

moveCol :: Int -> Position -> Position
moveCol n (f, l, c) = (f, l, c + n)

-- move down n lines, and go to the first column
moveRow :: Int -> Position -> Position 
moveRow  n (f, l, c) = (f, l + n, 1)


-- default input stream (aka fileName) name
defaultName :: String
defaultName = "input stream"

data LayoutContext = NoLayout 
                   | LayoutLevel Int
                     deriving (Show, Eq) -- TODO Ord too?

type ParseState = [LayoutContext]

{- TODO indentOfParseState
topLayoutLevel :: ParseState -> Int
topLayoutLevel (LayoutLevel n :_) = n
topLayoutLevel _ = 0
-}

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
      start2 =("puuuuu", 13, 13)

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

-- Monad for lexing (a 'continuation passing' monad)

newtype LexerM r a = LexerM { runL :: (a -> ParserM r) -> ParserM r }

instance Monad (LexerM r) where
    return a = LexerM $ \k -> k a
    LexerM v >>= f = LexerM $ \k -> v (\a -> runL (f a) k) 

getInput :: LexerM r AlexInput
getInput = LexerM $ \cont -> ParserM $ \r -> runP (cont r) r

-- skip some input characters (these must not include tabs or newlines).

skip :: Int -> LexerM r ()
skip n = LexerM $ \cont -> ParserM $ \r x -> runP (cont ()) (dropAI n r) (moveCol n x)
--skip = error "implement skip" -- TODO

dropAI :: Int -> AlexInput -> AlexInput
dropAI n (AlexInput p i) = AlexInput p (drop n i) 

{-
-- skip the next character, which must be a newline.

--skipNewline :: LexerM  r ()
--skipNewLine = LexerM $ \cont -> ParserM $ \r x -> runP (cont ()) (drop 1 r) (moveRow 1 x)
skipNewLine = error "implement skipNewLine" -- TODO


-- skip the next character, which must be a tab.

skipTab :: LexerM r ()
skipTab = error "implement skipTab" -- TODO

skipTab = LexerM $ 
          \cont -> ParserM $ 
                   \r x -> runP (cont ()) (drop (nextTab x) r) (moveCol (nextTab x) x)

nextTab :: Position -> Int
nextTab (_, _, x) = (tAB_LENGTH - (x-1) `mod` tAB_LENGTH)

tAB_LENGTH :: Int
tAB_LENGTH = 8
-}

-------------------- Lexer Part -- TODO correct this

data AlexInput = AlexInput {position :: !Position, input :: String} deriving (Show, Eq)
--data LexerState = LexerState {startCode :: !StartCode}
--type StartCode = Int
--type LexerAction = (AlexInput, String) -> StateT LexerState (Either String) (HasntToken, AlexInput)

--mkT :: HasntToken -> LexerAction
mkT :: Monad m => HasntToken -> m  HasntToken
mkT t = return t

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

