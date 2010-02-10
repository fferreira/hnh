module ParserMonad 
    (
      Position
    , showPosition
    , startingPosition -- TODO is this really needed?
    , LayoutContext(..)
    , ParseState
    , ParseResult(..)
    , ParserM(..)     -- TODO check if this should be exported
    , runParser , runParserWithFileName
    , returnOk , returnError
    , LexerM(..)      -- TODO check if this should be exported
    , getInput
    , skip
    , skipNewLine
    , skipTab
    )
    where

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

-- startingPosition is the begining of a file
startingPosition :: Position
startingPosition = (defaultName, 1, 0)


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
                              String     -- Input string  --TODO shouldn't we use an AlexInput
                              -> Position   -- Current postion
                              -> Position   -- Location of the last token read
                              -> ParseState -- layout info
                              -> ParseResult a
                            }

runParserWithFileName :: String -> ParserM a -> String -> ParseResult a
runParserWithFileName fileName (ParserM parse) program = parse program start start []
    where
      start = (fileName, 1, 0)

runParser :: ParserM a -> String -> ParseResult a
runParser = runParserWithFileName defaultName

returnOk :: a -> ParserM a
returnOk a = ParserM $ \_ _ _ state -> Ok state a

returnError :: String -> ParserM a
returnError msg = ParserM $ \_ pos _ _ -> Failed pos msg

instance Monad ParserM where
    return a = ParserM $ \_ _ _ {-input currPos lastPos-} state -> Ok state a 
    ParserM m >>= k = ParserM $ \input currPos lastPos state -> 
                case m input currPos lastPos state of
                  Failed pos msg -> Failed pos msg
                  Ok state' a -> runP (k a) input currPos lastPos state' 
                                 -- ^ continue the call 'chain'
    fail msg = ParserM $ \_ _ pos _ -> Failed pos msg


-- Monad for lexing (a 'continuation passing' monad)

newtype LexerM r a = LexerM { runL :: (a -> ParserM r) -> ParserM r }

instance Monad (LexerM r) where
    return a = LexerM $ \k -> k a
    LexerM v >>= f = LexerM $ \k -> v (\a -> runL (f a) k) 

getInput :: LexerM r String
getInput = LexerM $ \cont -> ParserM $ \r -> runP (cont r) r

-- skip some input characters (these must not include tabs or newlines).

skip :: Int -> LexerM r ()
skip n = LexerM $ \cont -> ParserM $ \r x -> runP (cont ()) (drop n r) (moveCol n x)

-- skip the next character, which must be a newline.

--skipNewline :: LexerM  r ()
skipNewLine = LexerM $ \cont -> ParserM $ \r x -> runP (cont ()) (drop 1 r) (moveRow 1 x)


-- skip the next character, which must be a tab.

skipTab :: LexerM r ()
skipTab = LexerM $ 
          \cont -> ParserM $ 
                   \r x -> runP (cont ()) (drop (nextTab x) r) (moveCol (nextTab x) x)

nextTab :: Position -> Int
nextTab (_, _, x) = (tAB_LENGTH - (x-1) `mod` tAB_LENGTH)

tAB_LENGTH :: Int
tAB_LENGTH = 8