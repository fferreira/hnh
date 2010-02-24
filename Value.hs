module Value
    (
     Value(..)
    )
    where

import Text.PrettyPrint.Leijen -- requires wl-pprint installed (available in cabal)
      
data Value = Value Int deriving (Show, Eq)

instance Pretty Value where
    pretty v = pretty $ show v --TODO improve this