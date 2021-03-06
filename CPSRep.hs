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
       , Name
       , AltK(..)
       , CondK(..)
       , Type(..)
       , LiteralValue(..)
       )
       where
import Syntax  

import Text.PrettyPrint.Leijen -- requires wl-pprint installed
import Control.Monad.State(State, put, get, runState)

data KExp = IfK Identifier KExp KExp
          | LitK LiteralValue Identifier KExp 
          | VarK Identifier Identifier KExp -- the second id is a new name
          -- D for deconstructors
            --    tuple      elem variable
          | TupDK Identifier Int Identifier KExp
          | ConDK Identifier Int Identifier KExp
            -- primitive operations
          | PrimK Name [Identifier] Identifier KExp
          | ConK Name [Identifier] Identifier KExp
          | AppK Identifier [Identifier]
            -- fun name, params, function body, after definition
          | FunK Identifier [Identifier] KExp KExp
          | TupleK [Identifier] Identifier KExp
          | ListK [Identifier] Identifier KExp
            
          | SwitchK [Identifier] [AltK]

          | HaltK Identifier
          deriving (Show, Eq)
                   
data CondK = WildK | CondK Name deriving (Show, Eq)
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
prettySExp (LitK v i k) = parens $ 
                          pretty "LitK" <+> pretty v <+> pid i 
                          <+> prettySExp k
prettySExp (VarK i i' k) = parens $ 
                           pretty "VarK" <+> pid i <+> pid i'
                           <+> prettySExp k 
prettySExp (AppK i ids) = parens $ 
                          pretty "Appk" <+> pid i 
                          <+> brackets (sep (map pid ids))
prettySExp (PrimK n ids i' k) = parens $ pretty "PrimK"                      
                             <+> pretty n <+> brackets (sep (map pid ids))
                             <+> pid i' <!> prettySExp k
prettySExp (ConK n ids i' k) = parens $ pretty "ConK" <+> pretty n
                               <+> brackets (sep (map pid ids))
                               <+> pid i'
                               <!> prettySExp k
                            
prettySExp (FunK fun params body cont) = parens $ pretty "Funk" 
                                         <!> pid fun
                                         <!> brackets(sep (map pid params))
                                         <!> prettySExp body
                                         <!> prettySExp cont
prettySExp (ListK ids i k) = parens $                                      
                             pretty "ListK" <+> sep (map pid ids)
                             <+> pid i
                             <+> prettySExp k
prettySExp (TupleK ids i k) = parens $                                      
                              pretty "TupleK" <+> brackets(sep (map pid ids))
                              <+> pid i
                              <+> prettySExp k
                             
prettySExp (SwitchK ids alts) = parens $ pretty "SwitchK"
                                <!> brackets(sep (map pid ids))
                                <!> (sep (map prettyAltK alts))
prettySExp (TupDK i n v k) = parens $ pretty "TupDK"                      
                             <+> pid i <> colon <> pretty n 
                             <+> pid v <+> prettySExp k
prettySExp (ConDK i n v k) = parens $ pretty "ConDK"                          
                             <+> pid i <> dot <> pretty n 
                             <+> pid v <+> prettySExp k

prettySExp (HaltK i) = parens $ pretty "***HaltK***" <+> parens (pid i)

prettyAltK (AltK conds k) = braces $ pretty "AltK"
                            <+> brackets(sep (map prettyCondK conds))
                            <+> prettySExp k
                            
prettyCondK WildK = pretty "_"                            
prettyCondK (CondK n) = parens (pretty "CondK" <+> pretty n)

pid (Id n num) = pretty (n++show num)

(<!>) a b = align (a <$> b)

