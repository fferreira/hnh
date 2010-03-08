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

module Token
    (
     HasntToken(..)
    )
    where

data HasntToken 
     = 
-- Special Characters

       LeftParen	-- (
     | RightParen	-- )
     | Comma		-- ,
     | SemiColon	-- ;
     | LeftSq		-- [
     | RightSq		-- ]
     | BackQuote	-- `
     | LeftCurly	-- {
     | RightCurly	-- }
     | Underscore       -- _
      
-- Reserved Words

     | CaseToken
     | DataToken
     | DefaultToken
     | ElseToken
     | IfToken
     | InToken
     | InfixToken
     | InfixlToken
     | InfixrToken
     | LetToken
     | OfToken
     | ThenToken
     | TypeToken

-- Reserved Operands

     | ColonOp		-- :
     | DoubleColonOp	-- ::
     | EqualsOp		-- =
     | BackSlashOp	-- \ One back slash
     | BarOp		-- |
     | RightArrowOp	-- ->
     | TildeOp		-- ~
     | TildeDotOp       -- ~.

-- Variable and Contructor names

     | VariableName 		String 
     | ConstructorName		String
     | VariableSymbol 		String 

-- Literals

     | IntegerLiteral 		Int
     | FloatLiteral 		Double 
     | StringLiteral 		String 
     | CharLiteral   		String	-- TODO should change to Char ?

-- CatchAll for errors

     | Unexpected               String

-- EOF token
     | EOFToken

     deriving (Eq, Show)
