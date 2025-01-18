module Interpreter.System.Optimizer
    (
        optimizeExpr,
        optimizeStatement,
        optimizeBody,
        optimizeProg
    )
where

import Utils.Data.Result
import Parser.Data.Ast
import Interpreter.Data.Environment
import Interpreter.System.Evaluator
import Interpreter.System.Operation
import Interpreter.System.Types
import Data.Maybe

optimizeExpr :: Env -> Expression -> Result String (Expression, Bool)
optimizeExpr e ex@(Variable n) = case fromEnv e n of
    Ok (EVariable _ _ v) -> Ok (v, True)
    _ -> Ok (ex, False)
optimizeExpr _ ex@(ELiteral _) = Ok (ex, False)
optimizeExpr e (BinaryOp op l r) = case mapBoth (optimizeExpr e) (l, r) of
    Err m -> Err m
    Ok ((l', lo), (r', ro)) -> if isStatic l' && isStatic r'
        then case evalBinOp op l' r' of
            Err m -> Err m
            Ok ex -> Ok (ex, True)
        else Ok (BinaryOp op l' r', lo || ro)
optimizeExpr e (UnaryOp op ex) = case optimizeExpr e ex of
    Err m -> Err m
    Ok (ex', oex) -> if isStatic ex'
        then case  evalUnaryOp op ex' of
            Err m -> Err m
            Ok ex'' -> Ok (ex'', True)
        else Ok (UnaryOp op ex', oex)
optimizeExpr e (Parenthesis ex) = case optimizeExpr e ex of
    Err m -> Err m
    Ok (ex', oex) -> if oex then Ok (ex', True) else Ok (ex, False)
optimizeExpr _ ex = Ok (ex, False)

optimizeStatement :: Env -> Statement -> Result String (Env, [Statement], Bool)
optimizeStatement e s@(VariableDeclaration n t v) = if isMutable t
    then case v of
        Nothing -> Ok (e, [s], False)
        Just ex -> do
            (ex', oex) <- optimizeExpr e ex
            return (e, [VariableDeclaration n t (Just ex')], oex)
    else do
        e' <- pushVariable e s
        return (e', [], True)
optimizeStatement e (FunctionDeclaration n p rt b) = do
    (b', ob) <- optimizeBodyMax (pushScope e []) False b
    return (e, [FunctionDeclaration n p rt b'], ob)
optimizeStatement e (WhileLoop cond b) = do
    (cond', ocond) <- optimizeExpr e cond
    return (e, [WhileLoop cond' b], ocond)
optimizeStatement e (If cond b eb) = do
    (cond', ocond) <- optimizeExpr e cond
    if isStatic cond'
        then if toBool cond'
            then Ok (e, b, True)
            else Ok (e, fromMaybe [] eb, True)
        else Ok (e, [If cond' b eb], ocond)
optimizeStatement e s@(TypeDeclaration {}) = Ok (e, [s], False)
optimizeStatement e (ReturnStatement ex) = do
    (ex', oex) <- optimizeExpr e ex
    return (e, [ReturnStatement ex'], oex)
optimizeStatement e (StandaloneFunctionCall n a) = do
    a' <- mapAll (optimizeExpr e) a
    return (e, [StandaloneFunctionCall n (map fst a')], any snd a')
optimizeStatement e (VariableReAssignment n ex) = do
    (ex', oex) <- optimizeExpr e ex
    return (e, [VariableReAssignment n ex'], oex)

optimizeBody :: Env -> Body -> Result String (Env, Body, Bool)
optimizeBody e [] = Ok (e, [], False)
optimizeBody e (s:xs) = do
    (e', sl, so) <- optimizeStatement e s
    (e'', xs', bo) <- optimizeBody e' xs
    return (e'', sl ++ xs', so || bo)

optimizeBodyMax :: Env -> Bool -> Body -> Result String (Body, Bool)
optimizeBodyMax e o b = do
    (e', b', ob) <- optimizeBody e b
    if ob then optimizeBodyMax e' True b' else Ok (b', o)

optimizeProg :: Program -> Result String Program
optimizeProg (Program b) = do
    (b', _) <- optimizeBodyMax env False b
    return (Program b')
