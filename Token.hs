module Token
    (
     HasntToken(..)
    )
    where

data HasntToken 
     = LineComment		String

-- Special Characters

     | LeftParen	-- (
     | RightParen	-- )
     | Comma		-- ,
     | SemiColon	-- ;
     | LeftSq		-- [
     | RightSq		-- ]
     | BackQuote	-- `
     | LeftCurly	-- {
     | RightCurly	-- }
      
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
     | WhereToken
     | UnusedReservedWord	String

-- Reserved Operands

     | DoubleDotOp	-- ..
     | ColonOp		-- :
     | DoubleColonOp	-- ::
     | EqualsOp		-- =
     | BackSlashOp	-- \ One back slash
     | BarOp		-- |
     | LeftArrowOp	-- <-
     | RightArrowOp	-- ->
     | AtOp		-- @
     | TildeOp		-- ~
     | DoubleArrowOp	-- =>

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

     deriving (Eq, Show)
