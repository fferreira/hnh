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
     | AtOp		-- @
     | TildeOp		-- ~

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
