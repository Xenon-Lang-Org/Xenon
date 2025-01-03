module Interpreter.Environment
    (
        Env(..),
        fromEnv,
        pushGlobal,
        pushLocal,
        pushEnv,
    )
where

import Interpreter.Scope
import Parser.Data.Ast
import Utils.Data.Result

data Env = Env { global :: Scope
               , local  :: Scope
               }

instance Show Env where
    show (Env g l) = 
        "-- Global --\n" ++ unlines (map show g) ++
        "\n-- Local --\n" ++ unlines (map show l)

fromEnv :: Env -> String -> Result String Statement
fromEnv (Env g l) name = case fromScope l name of
    Ok s -> Ok s
    _ -> fromScope g name

pushGlobal :: Env -> Statement -> Env
pushGlobal (Env g l) s = Env (scopePush g s) l

pushLocal :: Env -> Statement -> Env
pushLocal (Env g l) s = Env g (scopePush l s)

pushEnv :: Env -> Bool -> Statement -> Env
pushEnv env True s = pushGlobal env s
pushEnv env False s = pushLocal env s