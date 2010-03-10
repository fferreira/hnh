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
module PParser
{-  (
    parseHNH
  ) -}
  where

import Syntax
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as T
import Text.ParserCombinators.Parsec.Language(haskell)

import Control.Monad(liftM)


parseHNH :: String -> Either ParseError Program
parseHNH _ = Right $ Program []


--- Literals ---

int :: Parser LiteralValue
int = do i <- T.natural haskell
         return $ LiteralInt ((fromInteger i)::Int)

float :: Parser LiteralValue
float = liftM LiteralFloat (T.float haskell)


stringL :: Parser LiteralValue
stringL = do char '"'
             x <- many (noneOf "\"") -- TODO add support for escaped chars
             char '"'
             return $ LiteralString x
     
character :: Parser LiteralValue
character = do char '\''
               x <- many (noneOf "\'")
               char '\''
               return $ LiteralChar x


literal :: Parser LiteralValue
literal = try float
          <|> try int
          <|> try stringL
          <|> try character
          <?> "Literal"

--- Variables and Constructors ---

varid = do f <- many1 lower -- TODO should we avoid keywords?
           r <- many (alphaNum <|> (char '\''))
           return $ f ++ r

conid = do f <- many1 upper
           r <- many (alphaNum <|> (char '\''))
           return $ f ++ r
           
symbol = oneOf "!#$%&*+./<=>?@\\^|-~"           

varsym = many1 symbol

variable = parens varsym <|> varid
           
--- Utils ---           

parens = between (char '(') (char ')')
sqBrackets = between (char '[') (char ']')

  
--- Declarations ---  

typeDecl = do string "type" ; spaces 
              (n, params) <- simpleType ; spaces
              char '=' ; spaces
              t <- typeD ; spaces
              return $ TypeDcl n params t
              
dataDecl = do string "data" ; spaces              
              (n, params) <- simpleType ; spaces
              char '=' ; spaces
              cs <- constrs
              return $ DataDcl n params cs
           
           
varSpc = do v <- varid; spaces; return v
        
simpleType = do n <- conid
                spaces
                params <- many varSpc
                return (n, params)
                
--- 'data' declarations ---                
                
constrs = chainr1 constr barSep

barSep = do spaces ; char '|' ; spaces ; return (++)

constr = do n <- conid ; spaces                
            params <- many aTypeSpc
            return $ [ConDcl n params]
            
aTypeSpc = do spaces ; aType            
                
                
--- Types ---                

aConType = do n <- conid
              spaces
              params <- many typeSpc
              return $ ConType n params
              
aVarType = do spaces ; n <- varid ; spaces ; return $ VarType n              

typeSpc = do spaces ; t <- typeD; spaces; return t
             
             
sepBy2 p sep = do  x1 <- p
                   x2 <- (sep >> p)
                   xs <- many (sep >> p)
                   return (x1:x2:xs)
             
aTupleType = try $ parens (do ts <- typeD `sepBy2`(spaces >> char ',' >> spaces)
                              return $ TupleType ts)
             
aListType = do spaces
               t <- sqBrackets (do spaces; t <- typeD ; spaces ; return $ ConType "List" [t])
               spaces ; return t

aType = aConType
        <|> aVarType
        <|> aTupleType
        <|> aListType
        <|> parens aType
        
functionArrow = do spaces ; string "->" ; spaces ; return FuncType

typeD = chainr1 aType functionArrow 
        


