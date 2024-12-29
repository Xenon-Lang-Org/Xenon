module Interpreter.Environment
    (
        Env(..),
        fromEnv,
        pushGlobal,
        pushLocal,
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
fromEnv (Env l g) name = case fromScope l name of
    Ok s -> Ok s
    _ -> fromScope g name

pushGlobal :: Env -> Statement -> Env
pushGlobal (Env l g) s = Env (scopePush g s) l

pushLocal :: Env -> Statement -> Env
pushLocal (Env g l) s = Env g (scopePush l s)