module LexerHelper
    (
     AlexInput(..)
    ,LexerState(..)
    ,StartCode
    ,LexerA(..)
    ,mkT
    ,mkA
    ,alexGetChar
    ,alexInputPrevChar
    )
    where

import ParserMonad
import Token

import Control.Monad.State (StateT, {-evalStateT,-} get{-, put-})
import Control.Monad.Trans (lift)

data AlexInput = AlexInput {position :: !Position, input :: String} deriving (Show, Eq)
data LexerState = LexerState {startCode :: !StartCode}
type StartCode = Int
--type LexerAction = (AlexInput, String) -> StateT LexerState (Either String) (HasntToken, AlexInput)

data LexerA a = RegularAction a
              | Skip Int
              | SkipNewLine
              | SkipTab

--mkT :: HasntToken -> LexerAction
mkT :: Monad m => HasntToken -> m (LexerA HasntToken)  
mkT t = return (RegularAction t)

mkA :: Monad m => m (Maybe HasntToken)
mkA = error "no mkA"

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


