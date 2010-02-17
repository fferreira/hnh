module ParserMonad 
    (
--      Position
--    , showPosition
      LayoutContext(..) --TODO is it used?
    , ParseState       --TODO is it used ?
    , ParseResult(..) 
    , ParserM(..)
    , runParser , runParserWithFileName
    , returnOk , returnError
--    , AlexInput(..)
--    , alexGetChar
--    , alexInputPrevChar
    , lexer
    )
    where

import Token
import LexerUtils(AlexInput(..), Position)
import Lexer(getToken)

-- default input stream (aka fileName) name
defaultName :: String
defaultName = "input stream"

data LayoutContext = NoLayout 
                   | LayoutLevel Int
                     deriving (Show, Eq)

type ParseState = [LayoutContext]

data ParseResult a = Ok ParseState a 
                   | Failed Position String -- TODO add ParseState to error message?

instance (Show a) => Show (ParseResult a) where
    show (Ok _ a) = show a
    show (Failed p s) = "Error at:" ++ (show p) ++ "\nMessage: " ++ s

--  Monad for parsing
newtype ParserM a = ParserM { runP ::
                              AlexInput
                              -- v-- TODO neither position nor state are working!
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

-- Lexer

lexer :: (HasntToken -> ParserM a) -> ParserM a
lexer cont = ParserM $ \i ->
   do (token, i') <- getToken i
      case cont token of
          ParserM x -> x i'