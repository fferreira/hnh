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
module CPSRep
       (
         KExp(..)
       , Identifier(..)
       , AltK(..)
       , CondK(..)
       , Type(..)
       )
       where
import Syntax  

import Text.PrettyPrint.Leijen -- requires wl-pprint installed (available in cabal)
import Control.Monad.State(State, put, get, runState)

data KExp = IfK Identifier KExp KExp
          | LitK LiteralValue Identifier KExp Type
          | VarK Identifier Identifier KExp Type --TODO what is the second identifier
          -- D for deconstructors
            --    tuple      elem variable
          | TupDK Identifier Int Identifier KExp Type
          | ConDK Identifier Int Identifier KExp Type
            -- primitive operations
          | PrimK Identifier {-[Identifier]-} KExp Type
          | AppK Identifier [Identifier]
-- params, function body, prev res, after definition, type            
          | FunK [Identifier] KExp Identifier KExp
          -- | LetK [Identifier] [KExp] KExp -- unused
          | TupleK [Identifier] Identifier KExp
          | ListK [Identifier] Identifier KExp
            
          | SwitchK [Identifier] [AltK]

          | HaltK
          deriving (Show, Eq)
                   
data CondK = WildK | ConK Name deriving (Show, Eq)
data AltK = AltK [CondK] KExp deriving (Show, Eq)

instance Pretty KExp where                   
  pretty = prettySExp
  
instance Pretty AltK where
  pretty = prettyAltK

instance Pretty CondK where
  pretty = prettyCondK
  
prettySExp (IfK i k1 k2) = parens $ 
                       pretty "IfK" <+> pid i 
                       <+> prettySExp k1 <+> prettySExp k2
prettySExp (LitK v i k t) = parens $ 
                        pretty "LitK" <+> pretty v <+> pid i 
                        <+> prettySExp k -- <+> (brackets (pretty t))
prettySExp (VarK i i' k t) = parens $ 
                         pretty "VarK" <+> pid i <+> pretty i'
                         <+> prettySExp k -- <+> (brackets (pretty t))
prettySExp (AppK i ids) = parens $ 
                      pretty "Appk" <+> pid i 
                      <+> brackets (sep (map pid ids))
prettySExp (PrimK i k t) = parens $ pretty "PrimK"                      
                           <+> pid i <+> prettySExp k
                           -- <+> (brackets (pretty t))
prettySExp (FunK params body prev cont) = parens $ pretty "Funk" 
                                      <!> brackets(sep (map pid params))
                                      <!> prettySExp body
                                      <!> pid prev
                                      <!> prettySExp cont
prettySExp (ListK ids i k) = parens $                                      
                             pretty "ListK" <+> sep (map pid ids)
                             <+> pid i
                             <+> prettySExp k
prettySExp (TupleK ids i k) = parens $                                      
                              pretty "TupleK" <+> sep (map pid ids)
                              <+> pid i
                              <+> prettySExp k
                             
prettySExp (SwitchK ids alts) = parens $ pretty "SwitchK"
                                <!> brackets(sep (map pid ids))
                                <!> (sep (map prettyAltK alts))
prettySExp (TupDK i n v k t) = parens $ pretty "TupDK"                             
                               <+> pid i <> colon <> pretty n 
                               <+> pid v <+> prettySExp k
                               -- <+> (brackets (pretty t))
prettySExp (ConDK i n v k t) = parens $ pretty "ConDK"                             
                               <+> pid i <> dot <> pretty n 
                               <+> pid v <+> prettySExp k
                               -- <+> (brackets (pretty t))

prettySExp HaltK = parens $ pretty "***HaltK***"

prettyAltK (AltK conds k) = braces $ pretty "AltK"
                            <+> brackets(sep (map prettyCondK conds))
                            <+> prettySExp k
                            
prettyCondK WildK = pretty "_"                            
prettyCondK (ConK n) = parens (pretty "ConK" <+> pretty n)

pid (Id n num) = pretty (n++show num)

(<!>) a b = align (a <$> b)

