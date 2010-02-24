module Tools
    (
     traceVal
    ,traceP
    )
    where

import qualified Debug.Trace as D
import Text.PrettyPrint.Leijen -- requires wl-pprint installed (available in cabal)

traceVal :: Show a => a -> a
traceVal v = D.trace ("\ntrace:\n" ++ show v ++"\nEnd trace.\n") v
--traceVal v = v

traceP :: Pretty a => a -> a
traceP v = D.trace (show $ line<>pretty v<>line) v