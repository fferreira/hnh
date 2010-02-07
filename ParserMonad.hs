module ParserMonad 
    (
      Position
    , startingPosition -- TODO is this really needed?
    , LayoutContext(..)
    , ParseState
    , ParseResult(..)
    , P(..)
    , runParser , runParserWithFileName
    , returnOk , returnError
    )
    where

-- Position identifies a location in the source
type Position = (String, Int, Int) -- (File, line, col)

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
newtype P a = P { runP ::
                     String     -- Input string
                  -> Position   -- Current postion
                  -> Position   -- Location of the last token read
                  -> ParseState -- layout info
                  -> ParseResult a
                }

runParserWithFileName :: String -> P a -> String -> ParseResult a
runParserWithFileName fileName (P parse) program = parse program start start []
    where
      start = (fileName, 1, 0)

runParser :: P a -> String -> ParseResult a
runParser = runParserWithFileName defaultName

returnOk :: a -> P a
returnOk a = P $ \_ _ _ state -> Ok state a

returnError :: String -> P a
returnError msg = P $ \_ pos _ _ -> Failed pos msg

instance Monad P where
    return a = P $ \_ _ _ {-input currPos lastPos-} state -> Ok state a 
    P m >>= k = P $ \input currPos lastPos state -> 
                case m input currPos lastPos state of
                  Failed pos msg -> Failed pos msg
                  Ok state' a -> runP (k a) input currPos lastPos state' -- continue the call 'chain'
    fail msg = P $ \_ _ pos _ -> Failed pos msg