module TransformMonad
    (
     TransformM
    ,TransformResult(..)
    ,transformOk
    ,transformError
    ,runTransform
    ,nullTransform
    )
    where

import Text.PrettyPrint.Leijen(Doc, Pretty, pretty)

data TransformM a = TaM [Doc] (TransformResult a)

data TransformResult a = Ok a | Failed String

runTransform :: TransformM a -> (TransformResult a, [Doc])
runTransform (TaM l r) = (r, l)

transformOk :: Pretty a => a -> TransformM a
transformOk a = (TaM [pretty a] (Ok a))

transformError :: String -> TransformM a
transformError s = fail s

instance Monad TransformM where
    return t = (TaM [] (Ok t))
    (TaM l (Ok t)) >>= k = (TaM (l++l') t')
        where (TaM l' t') = k t
    (TaM l (Failed s)) >>= k = (TaM l (Failed s))
    
    fail msg = TaM [] (Failed msg)

nullTransform ::Pretty a => a -> TransformM a
nullTransform = transformOk