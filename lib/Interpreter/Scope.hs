module Interpreter.Scope
    (
        Scope,
        fromScope
    )
where

import Parser.Data.Ast
import Utils.Data.Result

type Scope = [Statement]

statementName :: Statement -> Result String String
statementName s = case s of
    VariableDeclaration n _ _ -> Ok n
    FunctionDeclaration n _ _ _ -> Ok n
    _ -> Err "Invalid Statement"

fromScope :: Scope -> String -> Result String Statement
fromScope [] n = Err $ n ++ " not found in current scope"
fromScope (x:xs) n = case statementName x of
    (Err msg) -> Err msg
    (Ok sn) | sn == n -> Ok x
    (Ok _) -> fromScope xs n

