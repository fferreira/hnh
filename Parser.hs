module Parser
    (
     parser,
     HasntSyntax(..),
    )
    where

import Lexer(HasntToken(..))

data HasntSyntax = ToBeDone String
                   deriving (Show, Eq)

parser :: [HasntToken] -> HasntSyntax
parser program =  undefined
