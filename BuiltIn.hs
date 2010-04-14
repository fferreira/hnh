module BuiltIn
       (
         EnvType
       , env0
       , idEnv0
       , builtInDecls
       , builtInContinuation
       , resultId
       )
       where

import Syntax
import CPSRep

import Control.Monad.State(State, get, put, evalState)

type EnvType = (Name, Type)
type IdEnvType = (Identifier, Type)

resultId = (Id "end" 0)

env0 :: [EnvType] -- the initial environment, containing all the builtin functions
env0 = 
    [("+", FunType (PrimType "Int") (FunType (PrimType "Int") (PrimType "Int")))
    {-,("-", FunType (PrimType "Int") (FunType (PrimType "Int") (PrimType "Int")))
    ,("*", FunType (PrimType "Int") (FunType (PrimType "Int") (PrimType "Int")))
    ,("/", FunType (PrimType "Int") (FunType (PrimType "Int") (PrimType "Int")))
    ,("^", FunType (PrimType "Int") (FunType (PrimType "Int") (PrimType "Int"))) -}
    ,("~", FunType (PrimType "Int") (PrimType "Int"))

    -- ,("==", FunType (PrimType "Int") (FunType (PrimType "Int") (DataType "Bool" [])))
    -- ,( ">", FunType (PrimType "Int") (FunType (PrimType "Int") (DataType "Bool" [])))
    -- ,( "<", FunType (PrimType "Int") (FunType (PrimType "Int") (DataType "Bool" [])))
    
    -- ,("+.", FunType (PrimType "Float") (FunType (PrimType "Float") (PrimType "Float")))
    -- ,("-.", FunType (PrimType "Float") (FunType (PrimType "Float") (PrimType "Float")))
    -- ,("*.", FunType (PrimType "Float") (FunType (PrimType "Float") (PrimType "Float")))
    -- ,("/.", FunType (PrimType "Float") (FunType (PrimType "Float") (PrimType "Float")))
    -- ,("^.", FunType (PrimType "Float") (FunType (PrimType "Float") (PrimType "Float")))

    -- ,("==.", FunType (PrimType "Float") (FunType (PrimType "Float") (DataType "Bool" [])))
    -- ,( ">.", FunType (PrimType "Float") (FunType (PrimType "Float") (DataType "Bool" [])))
    -- ,( "<.", FunType (PrimType "Float") (FunType (PrimType "Float") (DataType "Bool" [])))
    ]
    
idEnv0 :: [IdEnvType]
idEnv0 = zip (map 
              (\(n,num) -> (Id n num)) 
              (zip 
               (fst (unzip env0)) 
               [1..(length env0)]))
         (snd (unzip env0))
    
builtInDecls :: [Declaration]                
builtInDecls = [DataDcl 
                (DataType "List" [VarType "a"]) 
                [ConDcl "Cons" [VarType "a"
                               ,DataType "List" [VarType "a"]
                               ]
                ,ConDcl "Nil" []
                ]
               ,DataDcl
                (DataType "Bool" []) [ConDcl "True" []
                                     ,ConDcl "False" []
                                     ]
               ]
                
builtInContinuation :: KExp -> KExp
builtInContinuation k = k -- TODO complete
-- builtInContinuation k = build idEnv0 
--   where
--     build [(id, t)] = PrimK id k
--     build ((id, t):env) = PrimK id (build env)
    
-- type PrimSt = Int

-- newVar :: String -> State PrimSt Identifier
-- newVar var = do n <- get
--                 put (n+1)
--                 return (Id ("p_"++var) n)
-- newVars :: String -> Int -> State PrimSt [Identifier]
-- newVars var n = mapM (\n->newVar var) [1..n]

-- primToFun :: Identifier -> Int -> KExp -> State PrimSt KExp
-- primToFun _ 0 k = error "Unexpected primToFun call"
-- primToFun i n k = 
--   do params <- newVars "par" n
--      ptf params n k
--     where
--       ptf params 0 k = return (PrimK i {-params-} k)
--       ptf params n k = 
--         do f <- newVar "fun"
--            k' <- newVar "cont"
--            body <- ptf params (n-1) undefined -- cont
--            return (FunK f  [(params!!(length params - n)), k'] body k)
        
        
-- fun :: Identifier -> KExp -> KExp -> State PrimSt KExp
-- fun param body cont =
--   do fn <- newVar "fun"
--      k' <- newVar "cont"
--      return $ FunK fn [param, k'] (AppK k' []) cont

-- fun' fprev param cont = 
--   do f <- newVar "fun"
--      k' <- newVar "cont"
--      return $ FunK f [param, k'] (AppK fprev [k']) cont

-- primToFun' f params cont =
  -- xk <- newVar "algo"
  -- FunK f (params ++ k') (Prim f {-params -} k') cont

-- countParams :: Type -> Int
-- countParams t = countParams' t 0
--   where
--     countParams' :: Type -> Int -> Int
--     countParams' (FunType t1 t2) n = countParams' t1 (countParams' t2 (n+1))
--     countParams' _ n = n