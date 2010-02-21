module Tools
    (
     traceVal
    )
    where

import qualified Debug.Trace as D

traceVal :: Show a => a -> a
traceVal v = D.trace ("trace>>" ++ show v ++"<<") v
