module Interpreter.Evaluator
    (
        evalExpr,
        callEnv
    )
where

import Parser.Data.Ast
import Utils.Data.Result
import Interpreter.Environment
import Data.Foldable()
import Data.List()

evalExpr :: Env -> Expression -> Result String (Expression, Env)
evalExpr env expr = Ok (expr, env)

pushCallVar :: (Expression, (String, Type)) -> Result String Env -> Result String Env
pushCallVar _ (Err msg) = Err msg
pushCallVar (expr, (name, vtype)) (Ok env) = case evalExpr env expr of
    Ok (nexpr, nenv) -> 
        Ok $ pushLocal nenv (VariableDeclaration name vtype (Just nexpr))
    Err msg -> Err msg

callEnv :: Env -> Expression -> Statement -> Result String Env
callEnv (Env g _) (FunctionCall _ args) (FunctionDeclaration _ params _ _) =
    foldr pushCallVar (Ok (Env g [])) (zip args params) 
callEnv _ _ _ = Err "Failed to build call envrionment"