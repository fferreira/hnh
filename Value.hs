module Value
    (
     Value(..)
    )
    where

import Syntax

import Text.PrettyPrint.Leijen -- requires wl-pprint installed (available in cabal)
      
data Value = LitVal LiteralValue
           | ConVal Name -- a constructor, such as "True"
             deriving (Show, Eq)

instance Pretty Value where
    pretty v = pretty $ show v --TODO improve this