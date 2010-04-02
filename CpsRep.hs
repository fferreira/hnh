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
    along with HNH.  If not, see <http://www.gnu.org/licenses/>.
    
    Copyright 2010 Francisco Ferreira
-}
module CpsRep
       (
         KExp(..)
       , Identifier(..)
       )
       where
import Syntax  

import Text.PrettyPrint.Leijen -- requires wl-pprint installed (available in cabal)

data KExp = IfK Identifier KExp KExp
          | LitK LiteralValue Identifier KExp Type
          | VarK Identifier Identifier KExp Type --TODO what is the second identifier
          | AppK Identifier [Identifier]
-- params, function body, prev res, after definition, type            
          | FunK [Identifier] KExp Identifier KExp
          -- | LetK [Identifier] [KExp] KExp -- no used
          -- | TupleK
          -- | ListK
          | HaltK
          deriving (Show, Eq)
                   
instance Pretty KExp where                   
  pretty (IfK i k1 k2) = parens $ 
                         pretty "IfK" <+> pid i 
                         <+> pretty k1 <+> pretty k2
  pretty (LitK v i k t) = parens $ 
                          pretty "LitK" <+> pretty v <+> pid i 
                          <+> pretty k <+> (brackets (pretty t))
  pretty (VarK i k' k t) = parens $ 
                        pretty "VarK" <+> pid i <+> pretty k'
                        <+> pretty k <+> (brackets (pretty t))
  pretty (AppK i ids) = parens $ 
                        pretty "Appk" <+> pid i 
                        <+> sep (map (\i-> pretty i) ids)
  pretty (FunK params body prev cont) = parens $ pretty "Funk" 
                                        <!> brackets(sep (map (\i -> pretty i) params))
                                        <!> pretty body
                                        <!> pid prev
                                        <!> pretty cont
  pretty HaltK = parens $ pretty "***HaltK***"
  pretty e = pretty (show e) -- TODO improve this!


pid (Id n num) = pretty (n++show num)

(<!>) a b = align (a <$> b)