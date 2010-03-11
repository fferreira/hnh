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
module Parser
       (
         parseHNH
       ) 
       where

import Syntax
import TypeUtils(litToExp, assembleInfixOperator)
import ErrorMonad(ErrorM(..))

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as T
import Text.ParserCombinators.Parsec.Language(haskellDef)

import Control.Monad(liftM)


parseHNH :: String -> ErrorM Program
parseHNH p = case parse program "" p of
  (Right prog) -> return prog
  (Left err) -> fail (show err)


ut = UnknownType

--- Combinators ---

sepBy2 p sep = do  x1 <- p
                   x2 <- (sep >> p)
                   xs <- many (sep >> p)
                   return (x1:x2:xs)
--- hnh lexer ---
                   
hnh = T.makeTokenParser $ haskellDef
                          { T.identStart = lower
                          , T.reservedNames = hnhReservedWords                       
                          }

hnhReservedWords = ["let","in","case","of","if","then","else",
                    "data","type",
                    "infix","infixl","infixr"
                   ]

--- white space and comments ---

whiteSpace = T.whiteSpace hnh

ws = between whiteSpace whiteSpace -- adds spaces around a parser

trailWS = (\p -> do v <- p; whiteSpace ; return v) 

--- Literals ---

int :: Parser LiteralValue
int = do i <- T.natural hnh
         return $ LiteralInt ((fromInteger i)::Int)

float :: Parser LiteralValue
float = liftM LiteralFloat (T.float hnh)


stringL :: Parser LiteralValue
stringL = do char '"'
             x <- many (noneOf "\"") -- TODO add support for escaped chars
             char '"'
             whiteSpace
             return $ LiteralString x
     
character :: Parser LiteralValue
character = do char '\''
               x <- many (noneOf "\'")
               char '\''
               whiteSpace
               return $ LiteralChar x


literal :: Parser LiteralValue
literal = try float
          <|> try int
          <|> try stringL
          <|> try character
          <?> "Literal"
          
--- Variables and Constructors ---

varid = T.identifier hnh
{-
varid = do f <- many1 lower -- TODO should we avoid keywords?
           r <- many (alphaNum <|> (oneOf "'_"))
           whiteSpace
           return $ f ++ r
-}

conid = do f <- many1 upper
           r <- many (alphaNum <|> (oneOf "'_"))
           whiteSpace
           return $ f ++ r
           
symbol = oneOf "!#$%&*+./<=>?@\\^|-~"           

varsym = trailWS $ many1 symbol

variable = trailWS (parens varsym <|> varid <?> "variable")
           
--- Utils ---           

parens = between (char '(') (char ')')
sqBrackets = between (char '[') (char ']')

--- Program ---

program = do whiteSpace ; decls <- declarations ; whiteSpace ; eof ; return $ Program decls
  
--- Declarations ---  

declarations = many $ ws (do decl <- declaration 
                             optional (whiteSpace >> char ';')
                             whiteSpace
                             return decl
                           )

declaration = try typeDecl
              <|> try dataDecl
              <|> try typeSigDecl
              <|> try fixityDecl
              <|> try varDecl
              <|> try funDecl
--              <?> "declaration"

typeDecl = do string "type" ; whiteSpace 
              (n, params) <- simpleType ; whiteSpace
              char '=' ; whiteSpace
              t <- typeD ; whiteSpace
              return $ TypeDcl n params t
              
dataDecl = do string "data" ; whiteSpace              
              (n, params) <- simpleType ; whiteSpace
              char '=' ; whiteSpace
              cs <- constrs ; whiteSpace
              return $ DataDcl n params cs           
           
simpleType = do n <- conid
                whiteSpace
                params <- many varid
                whiteSpace
                return (n, params)
                
typeSigDecl = do vs <- (ws variable) `sepBy1` char ','
                 string "::" ; whiteSpace
                 t <- typeD
                 whiteSpace
                 return $ TypeSigDcl vs t

fixityDecl = do string "infix"                 
                a <- assocChar
                whiteSpace
                p <- (liftM charToInt $ digit) <|>  return 9
                ops <- (ws op) `sepBy1` char ','
                whiteSpace
                return $ FixityDcl a p ops 
                  where
                    charToInt :: Char -> Int
                    charToInt c = read [c]
                
assocChar = do char ' ' ; return NonAssoc
            <|> do char 'l' ; return LeftAssoc
            <|> do char 'r' ; return RightAssoc
               
op = do char '`' ; v <- varid ; char '`' ; return v
     <|> varsym
                
--- 'data' declarations ---                
                
constrs = chainr1 constr barSep

barSep = ws (char '|') >> return (++)

constr = do n <- conid ; whiteSpace                
            params <- many aType
            whiteSpace
            return $ [ConDcl n params]
            
--- Types ---                

aConType = do n <- conid
              whiteSpace
              params <- many typeD
              return $ ConType n params
              
aVarType = do n <- ws varid ; return $ VarType n              

             
aTupleType = try $ parens (do ts <- typeD `sepBy2`(whiteSpace >> char ',' >> whiteSpace)
                              return $ TupleType ts)
             
aListType = do whiteSpace
               t <- sqBrackets (do t <- (ws typeD) ; return $ ConType "List" [t])
               whiteSpace ; return t

aType = trailWS (aConType
                 <|> aVarType
                 <|> aTupleType
                 <|> aListType
                 <|> parens aType
                 <?> "type"
                )
        
functionArrow = ws (string "->") >> return FuncType

typeD = trailWS $ chainr1 aType functionArrow 
        

--- Variable and function declarations ---

varDecl = do p <- pattern ; ws (char '=') ; e <- expression ; (return $ PatBindDcl p e)
             
funDecl = do f <- variable ; whiteSpace
             ps <- many1 (trailWS pattern)
             ws (char '=')
             e <- expression
             return $ FunBindDcl f ps e ut

--- Patterns ---

varPat = do n <- variable ; return $ VarPat n ut
listPat = do h <- variable ; ws (char ':') ; t <- variable ; return $ ConPat "Cons" [h, t] ut
nilPat = char '[' >> whiteSpace >> char ']' >> (return $ ConPat "Nil" [] ut)
conPat = do cons <- conid ; vars <- many (ws variable) ; return $ ConPat cons vars ut
wildPat = char '_' >> (return $ WildcardPat ut) -- TODO add wildcards to other patterns too
tuplePat = do char '(' ; whiteSpace
              vs <- variable `sepBy2` (ws $ char ',')
              whiteSpace ; char ')'
              return $ TuplePat vs ut

pattern = trailWS (try listPat
                   <|> try nilPat
                   <|> try conPat
                   <|> try tuplePat
                   <|> try wildPat
                   <|> try varPat
                   <|> try (parens pattern)
                   <?> "pattern"
                  )

--- Expresions ---

expression = expr -- TODO eliminate

expr = ws expi
       <?> "expression"

expi :: Parser Exp
expi = try minusExp
       <|> try minusFloatExp
       <|> do e1 <- exp10 ; whiteSpace -- TODO add ~ and ~.
              opExp e1

minusExp = do char '~'
              e <- ws expi
              return $ MinusExp e ut
              
minusFloatExp = do string "~."
                   e <- ws expi
                   return $ MinusFloatExp e ut

opExp e1 = (do o <- op ; e2 <- (ws expi) ; return $ assembleInfixOperator e1 o e2) 
             <|> return e1
             
exp10 :: Parser Exp
exp10 = try lambdaExp
        <|> try letExp
        <|> try ifExp
        <|> try caseExp
        <|> try fExp

lambdaExp = do char '\\'
               pats <- many1 (ws pattern)
               ws (string "->")
               e <- expr
               return $ LambdaExp pats e ut

letExp = do string "let"
            ws $ optional (char '{')
            decls <- ws declarations
            ws $ optional (char '}')
            string "in"
            e <- expr
            return $ LetExp decls e ut

ifExp = do  string "if"
            e1 <- ws expr
            string "then"
            e2 <- ws expr
            string "else"
            e3 <- ws expr
            return $ IfExp e1 e2 e3 ut

caseExp = do string "case"
             e <- ws expr
             string "of"
             ws $ optional (char '{')
             alts <- alt `sepBy` whiteSpace
             ws $ optional (char '}')
             return $ CaseExp [e] alts ut
             
alt = do p <- pattern             
         ws $ string "->"
         e <- expr
         optional (ws $ char ';')
         return $ Alternative [p] e
             
fExp = do es <- many1 aExp
          return $ foldl (\e1 e2 ->FExp e1 e2 ut) (head es) (tail es)

aExp :: Parser Exp
aExp = try (do n <- conid ; return (ConExp n ut))
       <|> try (do l <- literal ; return (litToExp l))
       <|> try listExp
       <|> try parensExp
       <|> try tupleExp
       <|> try (do n <- variable ; return (VarExp n ut))
       <?> "Expr"
       
parensExp = do whiteSpace ; char '(' ; whiteSpace       
               e <- expr
               t <- (try (string "::" >> whiteSpace >> typeD)) <|> return ut
               ws (char ')')
               return $ ParensExp e t

tupleExp = do char '(' ; whiteSpace
              es <- expr `sepBy2` (ws $ char ',')
              whiteSpace ; char ')'
              return $ TupleExp es ut
              
listExp = do char '[' ; whiteSpace
             es <- expr `sepBy` (ws $ char ',')
             whiteSpace ; char ']'
             return $ ListExp es ut              