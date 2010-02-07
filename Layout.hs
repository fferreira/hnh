module Layout
    (
     layout
    )
    where

import Token
import ParserMonad(Position)

{-
The layout function returns a layout-insensitive translation of tokens and removes
the position information from the list of tokens.
-}

layout :: [(Position, HasntToken)] -> [HasntToken]
-- layout tokens = map (\(p, t) -> t) tokens -- trivial null implementation

-- even if the columns are numbered from 1, we start with 0 because in the begining there are no blocks to close.
layout tokens = lay tokens [] [0] 

{-

The layout rules:

    * let, where, do, and of begin blocks <<-- is this really needed?
    * same indentation as the first in the block means a new def/stmt/alt (adds a ;)
    * more indentation means that this line belongs to the last line
    * less indentation means end of a block of definitions/statements/alternatives 

The layout rule has a special case for let expressions so that the keyword in also ends the block of definitions

Note:
Braces "{}" must not be added if there is already one (TODO IMPLEMENT THIS)
Idea: use different "braces" for literal (inserted by the user) and those
      inserted by this function

Improvements to lay:
    * col should not be a stack, the call stack should be used
    * support the { originally in the code (see the note)
-}


lay :: [(Position, HasntToken)] -> [HasntToken] -> [Int] -> [HasntToken]

-- When there are no more tokens to process, we must close all remaining braces
lay [] tokens col = 
    (reverse tokens) ++ (replicate ((length col) - 1) (LeftCurly)) 

-- We open a new brace for the required keywords, as mentioned in the layout rules
-- TODO avoid the "copy & paste" programming here

lay ((pos, LetToken):xs) tokens col = 
    lay xs (LeftCurly : LetToken : tokens)  (push (getColOfFirstElement xs) col)

-- The special case for the in keyword
lay ((_,  InToken):xs) tokens col = lay xs (InToken : RightCurly : tokens) (pop col)

-- Process all the tokens, adding ; and closing the braces where needed
lay (((_, _, tokenCol), tok):xs) tokens col = 
    case (tokenCol `compare` (head col)) of
      (LT) -> lay xs (RightCurly : tok : tokens) (pop col)
      (EQ) -> lay xs (tok : SemiColon : tokens) col
      (GT) -> lay xs (tok : tokens) col -- when the token doesn't change the layout


-- Some utilities

push :: a -> [a] -> [a]
push e l = e:l

pop :: [a] -> [a]
pop = tail 

getColOfFirstElement (((_, _, col), _):_) = col



