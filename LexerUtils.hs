{-
  This file is part of HNH.

    HNH is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    HNH is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Foobar.  If not, see <http://www.gnu.org/licenses/>.
    
    Copyright 2010 Francisco Ferreira
-}

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

