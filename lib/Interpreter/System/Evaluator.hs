module Interpreter.System.Evaluator
    (
        MEval,
        evalExpr,
        evalStatement,
        evalBody,
        evalBodyPrint,
        evalProg,
        isStatic
    )
where

import Parser.Data.Ast
import Utils.Data.Result
import Interpreter.Data.Environment
import Interpreter.System.Operation
import Interpreter.System.Types
import Data.Foldable()
import Data.Maybe (fromMaybe)
import Utils.System.Print

type Operands = (Expression, Expression)
type Eval = (Env, Expression)
type MEval = (Env, Maybe Expression)

-- Expression

evalOperand :: Int -> Env -> Operands -> Result String (Env, Operands)
evalOperand s e (l, r) = case evalSizedExpr s e l of
    Ok (e', l') -> case evalSizedExpr s e' r of
        Ok (e'', r') -> Ok (e'', (l', r'))
        Err msg -> Err msg
    Err msg -> Err msg

evalSizedExpr :: Int -> Env -> Expression -> Result String Eval
evalSizedExpr s e expr = case expr of
    Variable n -> case fromEnv e n of
        Ok (EVariable _ _ v) -> Ok (e, v)
        Ok _ -> Err $ n ++ " is not a variable"
        Err msg -> Err msg

    ELiteral l -> Ok (e, ELiteral l)

    BinaryOp op l r -> case evalOperand s e (l, r) of
        Ok (e', (l', r')) -> both (Ok e', evalSizedBinOp s op l' r')
        Err msg -> Err msg

    UnaryOp op expr' -> case evalSizedExpr s e expr' of
        Err m -> Err m
        Ok (e', expr'') -> case evalUnaryOp op expr'' of
            Err m -> Err m
            Ok res -> Ok (e', res)

    Parenthesis expr' -> evalSizedExpr s e expr'

    FunctionCall n a -> callFunc s e n a

evalExpr :: Env -> Expression -> Result String Eval
evalExpr = evalSizedExpr 0

evalAllExpr :: Int -> Env -> [Expression] -> Result String (Env, [Expression])
evalAllExpr s = buildUp (evalSizedExpr s)

evalCastExpr :: Env -> Type -> Expression -> Result String Eval
evalCastExpr e t expr = case evalSizedExpr (typeSize t) e expr of
    Ok (e', expr') -> case castExpr e' expr' t of
        Ok expr'' -> Ok (e', expr'') 
        Err m -> Err m
    Err m -> Err m

isStatic :: Expression -> Bool
isStatic (Variable {}) = False
isStatic (ELiteral {}) = True
isStatic (BinaryOp _ l r) = isStatic l && isStatic r 
isStatic (UnaryOp _ ex) = isStatic ex
isStatic (Parenthesis ex) = isStatic ex
isStatic (FunctionCall {}) = False

-- Statement

evalStatement :: Env -> Statement -> Result String MEval
evalStatement e st = case st of

    WhileLoop cond body -> evalWhile e cond body

    If c b eb -> case evalExpr e c of
        Ok (e', c') -> if toBool c' then evalBody e' b else
            case eb of
                Just eb' -> evalBody e' eb'
                _ -> Ok (e', Nothing)
        Err m -> Err m

    ReturnStatement ret -> case evalExpr e ret of
        Ok (e', ret') -> Ok (e', Just ret')
        Err msg -> Err msg

    StandaloneFunctionCall n a -> case callFunc 0 e n a of
        Ok (e', _) -> Ok (e', Nothing)
        Err m -> Err m

    VariableReAssignment n v -> case fromEnv e n of
        Ok (EVariable _ t _) -> case evalCastExpr e t v of
            Ok (e', v') -> both (assignVar e' n v', Ok Nothing)
            Err m -> Err m
        Ok _ -> Err (n ++ " is not assignable")
        Err m -> Err m

    _ -> declare e st `andThen` (Ok . pairR Nothing)

pairR :: a -> e -> (e, a)
pairR x y = (y, x)

zeroExpr :: Expression
zeroExpr = ELiteral $ IntLiteral 0

declareVariable :: Env -> String -> Type -> Maybe Expression -> Result String Env
declareVariable e n t (Just v) = case evalSizedExpr (typeSize t) e v of
    Ok (e', v') -> case castExpr e v' t of
        Ok v'' -> pushVariable e' (VariableDeclaration n t (Just v''))
        Err msg -> Err msg
    Err msg -> Err msg
declareVariable e n t Nothing = case defaultExpr e t of
    Ok v -> pushVariable e (VariableDeclaration n t (Just v))
    Err msg -> Err msg

declare :: Env -> Statement -> Result String Env
declare e (FunctionDeclaration n p t body) =
    pushFunction e (FunctionDeclaration n p t body)
declare e (VariableDeclaration n t v) = declareVariable e n t v
declare e (TypeDeclaration m t) = pushType e (TypeDeclaration m t)
declare _ _ = Err "Invalid declaration"

evalWhile :: Env -> Expression -> Body -> Result String MEval
evalWhile e c b = case evalExpr e c of
    Err m -> Err m
    Ok (e', c') -> if not (toBool c')
        then Ok (e', Nothing)
        else case evalBody e' b of
            Ok (e'', Nothing) -> evalWhile e'' c b
            Ok r -> Ok r
            Err m -> Err m

-- Body

evalBody :: Env -> Body -> Result String MEval
evalBody e [] = Ok (e, Nothing)
evalBody e (x:xs) = case evalStatement e x of
    Ok (e', Nothing) -> evalBody e' xs
    Ok r -> Ok r
    Err msg -> Err msg

evalBodyPrint :: Env -> Body -> IO Env
evalBodyPrint e [] = return e
evalBodyPrint e (StandaloneFunctionCall n a:xs) = case callFunc 0 e n a of
    Ok (e', r) -> print r >> evalBodyPrint e' xs
    Err m -> printFailure m >> return e
evalBodyPrint e (x:xs) = case evalStatement e x of
    Ok (e', _) -> evalBodyPrint e' xs
    Err m -> printFailure m >> return e

ensureEval :: Result String MEval -> Result String Eval
ensureEval (Ok (e, Nothing)) = Ok (e, zeroExpr)
ensureEval (Ok (e, Just ret)) = Ok (e, ret)
ensureEval (Err msg) = Err msg

ensuredEvalBody :: Env -> Body -> Result String Eval
ensuredEvalBody e b = ensureEval $ evalBody e b

evalProg :: Program -> Result String (Env, Expression)
evalProg (Program b) = ensuredEvalBody env b

-- Function

callScope :: Env -> [Expression] -> [(String, Type)] -> Result String Scope
callScope e args params 
    | length args /= length params = Err "Invalid number of arguments"
    | otherwise = buildScope $ zip args params
    where
        buildScope [] = Ok []
        buildScope ((v,(n, t)):xs) = case castExpr e v t of
            Err m -> Err m
            Ok v' -> case buildScope xs of
                Err m -> Err m
                Ok vs -> Ok (EVariable n t v':vs)

callEnv :: Env -> [Expression] -> [(String, Type)] -> Result String Env
callEnv e args params = case callScope e args params of
    Ok s -> Ok $ pushScope e s
    Err m -> Err m

callFunc :: Int -> Env -> String -> [Expression] -> Result String Eval
callFunc s e n args = do
    func <- fromEnv e n
    case func of
        EFunction _ params rt body -> do
            (e', args') <- evalAllExpr s e args
            ce <- callEnv e' args' params
            (ce', expr') <- evalBody ce body
            cexpr <- castExpr ce' (fromMaybe zeroExpr expr') rt
            return (popScope ce', cexpr)
        _ -> Err $ n ++ " is not callable"
