module Interpreter.System.Evaluator
    (
        evalExpr,
        evalStatement,
        evalBody,
        evalProg
    )
where

import Parser.Data.Ast
import Utils.Data.Result
import Interpreter.Data.Environment
import Interpreter.System.BinaryOperation
import Interpreter.System.Types
import Data.Foldable()
import Data.Maybe (fromMaybe)

type Operands = (Expression, Expression)
type Eval = (Env, Expression)
type MEval = (Env, Maybe Expression)
type EAcc = (Env -> Expression -> Result String Eval)

-- Expression

evalOperand :: Env -> Operands -> Result String (Env, Operands)
evalOperand e (l, r) = case evalExpr e l of
    Ok (e', l') -> case evalExpr e' r of
        Ok (e'', r') -> Ok (e'', (l', r'))
        Err msg -> Err msg
    Err msg -> Err msg

evalExpr :: Env -> Expression -> Result String Eval
evalExpr e expr = case expr of
    Variable n -> case fromEnv e n of
        Ok (EVariable _ t v) -> case toLiteralExpr t v of
            Ok l -> Ok (e, ELiteral l)
            Err msg -> Err msg
        Ok _ -> Err $ n ++ " is not a variable"
        Err msg -> Err msg

    ELiteral l -> Ok (e, ELiteral l)

    BinaryOp Assign (Variable n) r -> case evalExpr e r of
        Ok (e', r') -> evalBinOp e' Assign (Variable n) r'
        Err msg -> Err msg

    BinaryOp op l r -> case evalOperand e (l, r) of
        Ok (e', (l', r')) -> evalBinOp e' op l' r'
        Err msg -> Err msg

    UnaryOp _ _ -> Err "Unary operator are not supported yet"

    Parenthesis expr' -> evalExpr e expr'

    FunctionCall fname args -> callFunc e fname args

-- Statement

evalStatement :: Env -> Statement -> Result String MEval
evalStatement e st = case st of
    ExpressionStatement expr -> case evalExpr e expr of
        Ok (e', _) -> Ok (e', Nothing)
        Err msg -> Err msg

    WhileLoop cond body -> evalWhile e cond body

    Conditional {} -> Err "Not implemented"

    ReturnStatement ret -> case evalExpr e ret of
        Ok (e', ret') -> Ok (e', Just ret')
        Err msg -> Err msg

    _ -> declare e st `andThen` (Ok . pairR Nothing)

pairR :: a -> e -> (e, a)
pairR x y = (y, x)

zeroExpr :: Expression
zeroExpr = ELiteral $ IntLiteral 0

declare :: Env -> Statement -> Result String Env
declare e (FunctionDeclaration name p t body) =
    pushFunction e (FunctionDeclaration name p t body)
declare e (VariableDeclaration name t (Just expr)) = case evalExpr e expr of
    Ok (e', expr') -> case castExpr e expr' t of
        Ok expr'' -> pushVariable e' (VariableDeclaration name t (Just expr''))
        Err msg -> Err msg
    Err msg -> Err msg
declare e (TypeDeclaration t) = pushType e (TypeDeclaration t)
declare _ _ = Err "Invalid declaration"

evalLoop :: EAcc -> Expression -> Body -> MEval -> Result String MEval
evalLoop _ _ _ (e, Just ret) = Ok (e, Just ret)
evalLoop f cond body (e, Nothing) = case f e cond of
    Ok (e', cond') -> case toBool cond' of
        Ok False -> Ok (e, Nothing)
        Err msg -> Err msg
        _ -> case evalBody e' body of 
            Ok (e'', Nothing) -> evalLoop f cond body (e'', Nothing)
            Err msg -> Err msg
            Ok ret -> Ok ret
    Err msg -> Err msg

evalWhile :: Env -> Expression -> Body -> Result String MEval
evalWhile e cond body = evalLoop evalExpr cond body (e, Nothing)

-- Body

evalBody :: Env -> Body -> Result String MEval
evalBody e [] = Ok (e, Nothing)
evalBody e (x:xs) = case evalStatement e x of
    Ok (e', Nothing) -> evalBody e' xs
    Ok r -> Ok r
    Err msg -> Err msg

ensureEval :: Result String MEval -> Result String Eval
ensureEval (Ok (e, Nothing)) = Ok (e, zeroExpr)
ensureEval (Ok (e, Just ret)) = Ok (e, ret)
ensureEval (Err msg) = Err msg

evalProg :: Program -> Result String (Env, Expression)
evalProg (Program body) = ensureEval $ evalBody (env True) body

-- Function

appendCallVar :: Env -> Env -> Expression -> String -> Type -> Result String (Env, Env)
appendCallVar old new expr name t = case castExpr old expr t of
    Ok expr' -> case pushVariable new (VariableDeclaration name t (Just expr')) of
        Ok new' -> Ok (old, new')
        Err msg -> Err msg
    Err msg -> Err msg

pushCallVar :: (Expression, (String, Type)) -> Result String (Env, Env) -> Result String (Env, Env)
pushCallVar _ (Err msg) = Err msg
pushCallVar (expr, (name, t)) (Ok (old, new)) = case evalExpr old expr of
    Ok (old', expr') -> appendCallVar old' new expr' name t
    Err msg -> Err msg

callEnv :: Env -> [Expression] -> [(String, Type)] -> Result String Env
callEnv e args params =
    case foldr pushCallVar (Ok (e, fromGlobal False (global e))) (zip args params) of
        Ok (_, new) -> Ok new
        Err msg -> Err msg

callFunc :: Env -> String -> [Expression] -> Result String (Env, Expression)
callFunc e name args = do
    func <- fromEnv e name
    case func of
        EFunction _ params _ body -> do
            e' <- callEnv e args params
            (e'', expr') <- evalBody e' body
            return (setGlobalScope e (global e'') , fromMaybe zeroExpr expr')
        _ -> Err $ name ++ " is not callable"