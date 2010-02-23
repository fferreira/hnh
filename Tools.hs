module Tools
    (
     traceVal
    )
    where

import qualified Debug.Trace as D

traceVal :: Show a => a -> a
--traceVal v = D.trace ("\ntrace:\n" ++ show v ++"\nEnd trace.\n") v
traceVal v = v