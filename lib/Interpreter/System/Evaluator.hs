module Interpreter.System.Evaluator
    (
        -- evalExpr,
        -- evalStatement,
        -- evalStatementList,
        -- evalProg
    )
where

-- import Parser.Data.Ast
-- import Utils.Data.Result
-- import Interpreter.Environment
-- import Data.Foldable()
-- import Data.List()
-- import Interpreter.BinOp
-- import Interpreter.Types

-- Expression

-- evalExpr :: Env -> Expression -> Result String (Env, Expression)
-- evalExpr env expr = case expr of
--     Variable n -> case fromEnv env n of
--         Ok (EVariable _ t v) -> case toLiteralExpr env t v of
--             Ok nexpr -> Ok (env, nexpr)

--     ELiteral l -> Ok (env, ELiteral l)

--     BinaryOp op l r -> case evalOperands (env, l, r) of
--         Ok (nenv, el, er) -> evalBinOp 

--     UnaryOp _ _ -> Err "Unary operator are not supported yet"

--     FunctionCall fname args -> callFunc env fname args

-- -- Statement

-- declareVar :: Env -> Bool -> String -> Type -> Maybe Expression -> Result String Env
-- declareVar env gl name vtype Nothing =
--     Ok $ pushEnv env gl (VariableDeclaration name vtype Nothing)
-- declareVar env gl name vtype (Just expr) = case evalExpr env expr of
--     Ok (nenv, nexpr) -> case castExpr nexpr vtype of
--         Ok cexpr ->
--             Ok $ pushEnv nenv gl (VariableDeclaration name vtype (Just cexpr))
--         Err msg -> Err msg
--     Err msg -> Err msg

-- evalStatement :: Env -> Bool -> Statement -> Result String Env
-- evalStatement env gl st = case st of
--     VariableDeclaration name vtype val -> declareVar env gl name vtype val

--     FunctionDeclaration name params rt body ->
--         Ok $ pushEnv env gl (FunctionDeclaration name params rt body)
    
--     ExpressionStatement expr -> case evalExpr env expr of
--         Ok (nenv, _) -> Ok nenv
--         Err msg -> Err msg

-- -- Program

-- evalStatementList :: Env -> Bool -> [Statement] -> Result String (Env, Expression)
-- evalStatementList env _ [] = Ok (env, ELiteral $ IntLiteral 0)
-- evalStatementList env _ [ExpressionStatement expr] = evalExpr env expr
-- evalStatementList env gl (x:xs) = case evalStatement env gl x of
--     Ok nenv -> evalStatementList nenv gl xs
--     Err msg -> Err msg

-- evalProg :: Program -> Result String (Env, Expression)
-- evalProg (Program body) = evalStatementList (Env [] []) True body

-- -- Function

-- pushCallVar :: (Expression, (String, Type)) -> Result String (Env, Env) -> Result String (Env, Env)
-- pushCallVar _ (Err msg) = Err msg
-- pushCallVar (expr, (name, vtype)) (Ok (old, new)) = case evalExpr old expr of
--     Ok (nold, nexpr) -> case castExpr nexpr vtype of
--         Ok cexpr -> Ok (nold, pushLocal new (VariableDeclaration name vtype (Just cexpr)))
--         Err msg -> Err msg
--     Err msg -> Err msg

-- callEnv :: Env -> [Expression] -> [(String, Type)] -> Result String Env
-- callEnv (Env g l) args params =
--     case foldr pushCallVar (Ok (Env g l, Env g [])) (zip args params) of
--         Ok (_, new) -> Ok new
--         Err msg -> Err msg

-- callFunc :: Env -> String -> [Expression] -> Result String (Env, Expression)
-- callFunc env name args = case fromEnv env name of
--     Ok (FunctionDeclaration _ params _ body) -> 
--         case callEnv env args params of
--             Ok nenv -> case evalStatementList nenv False body of
--                 Ok (_, rexpr) -> Ok (env, rexpr)
--                 Err msg -> Err msg
--             Err msg -> Err msg
--     Ok (VariableDeclaration {}) -> Err $ name ++ " is not callable"
--     Err msg -> Err msg
--     _ -> Err $ name ++ " undefined"
