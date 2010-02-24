module ErrorMonad
    (
     ErrorM(..)
    )
    where

import Text.PrettyPrint.Leijen -- requires wl-pprint installed (available in cabal)

data ErrorM a = Success a | Error String

instance Monad ErrorM where
    return t = Success t
    (Success a) >>= k = k a
    (Error msg) >>= k = Error msg
    fail msg = Error msg

instance Pretty a => Pretty (ErrorM a) where
    pretty (Error msg) = pretty "Error"<> colon <+> pretty msg
    pretty (Success a) = pretty a