module Interpreter.Scope
    (
        Scope,
        fromScope,
        scopePush
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
fromScope [] n = Err $ n ++ " is undefined"
fromScope (x:xs) n = case statementName x of
    (Err msg) -> Err msg
    (Ok sn) | sn == n -> Ok x
    (Ok _) -> fromScope xs n

scopePush :: Scope -> Statement -> Scope
scopePush scope s = case s of
    VariableDeclaration {} -> s : scope
    FunctionDeclaration {} -> s : scope
    _ -> scope
