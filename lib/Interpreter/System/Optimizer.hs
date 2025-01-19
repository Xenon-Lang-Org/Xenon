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

-- Function

exprInlineable :: Env -> [Field] -> Expression -> Bool
exprInlineable _ _ (ELiteral _) = True
exprInlineable e p (BinaryOp _ l r) = exprInlineable e p l && exprInlineable e p r
exprInlineable e p (UnaryOp _ ex) = exprInlineable e p ex
exprInlineable e p (Parenthesis ex) = exprInlineable e p ex
exprInlineable e p (FunctionCall n a) = isOk (getFunction e n) && all (exprInlineable e p) a
exprInlineable _ p (Variable n) = isParam p
    where
        isParam [] = False
        isParam ((pn, _):xs) = n == pn || isParam xs

funcInlineable :: Env -> [Field] -> Body -> Bool
funcInlineable e p b = paramsInlineable p && bodyInlineable b
    where
        paramsInlineable [] = True
        paramsInlineable ((_, t):xs) = not (isMutable t) && paramsInlineable xs
        bodyInlineable [ReturnStatement ex] = exprInlineable e p ex
        bodyInlineable _ = False

callParamScope :: [Field] -> [Expression] -> Result String Scope
callParamScope [] [] = Ok []
callParamScope [] (_:_) = Err "Invalid number of arguments in function call"
callParamScope (_:_) [] = Err "Invalid number of arguments in function call"
callParamScope ((n, t):ps) (ex:exs) = do
    s <- callParamScope ps exs
    return (EVariable n t ex : s)

substitueExpr :: Env -> Expression -> Expression
substitueExpr e ex@(Variable n) = unwrapOr ex (getVarValue e n)
substitueExpr _ ex@(ELiteral _) = ex
substitueExpr e (BinaryOp op l r) = BinaryOp op (substitueExpr e l) (substitueExpr e r)
substitueExpr e (UnaryOp op ex) = UnaryOp op (substitueExpr e ex)
substitueExpr e (Parenthesis ex) = Parenthesis $ substitueExpr e ex
substitueExpr e (FunctionCall n a) = FunctionCall n (map (substitueExpr e) a)

inlineFuncCall :: Env -> Body -> Result String Expression
inlineFuncCall e [ReturnStatement ex] = Ok $ substitueExpr e ex
inlineFuncCall _ _ = Err "Failed to inline inlineable function"

optimizeFuncCall :: Env -> String -> [Expression] -> Result String (Expression, Bool)
optimizeFuncCall e n a = do
    a' <- mapAll (optimizeExpr e) a
    case fromEnv e n of
        Ok (EFunction _ p _ b) -> do
            s <- callParamScope p a
            ex <- inlineFuncCall (pushScope e s) b
            return (ex, True)
        _ -> Ok (FunctionCall n (map fst a'), all snd a')

addInlinableFunc :: Env -> Statement -> (Env, Bool)
addInlinableFunc e (FunctionDeclaration n p rt b)
    | funcInlineable e p b = case fromEnv e n of
        Ok _ -> (e, False)
        _ -> (pushEnv e (EFunction n p rt b), True)
addInlinableFunc e _ = (e, False)

-- Expression

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
optimizeExpr e (FunctionCall n a) = optimizeFuncCall e n a

-- Statement

allBranchesReturn :: Body -> Bool
allBranchesReturn [] = False
allBranchesReturn (ReturnStatement {}:_) = True
allBranchesReturn (If _ b Nothing:xs) = all allBranchesReturn [b, xs]
allBranchesReturn (If _ b (Just eb):xs) = all allBranchesReturn [b, eb, xs]
allBranchesReturn (WhileLoop _ b:xs) = all allBranchesReturn [b, xs]
allBranchesReturn (_:xs) = allBranchesReturn xs

nothingIfEmpty :: [a] -> Maybe [a]
nothingIfEmpty [] = Nothing
nothingIfEmpty x = Just x

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
    (_, b', ob) <- optimizeBodyMax (pushScope e []) False b
    let (e', oe) = addInlinableFunc e (func b')
    return (e', [func b'], ob || oe)
        where
            func = FunctionDeclaration n p rt

optimizeStatement e (WhileLoop cond b) = do
    (cond', ocond) <- optimizeExpr e cond
    return (e, [WhileLoop cond' b], ocond)
optimizeStatement e (If cond b eb) = do
    (cond', ocond) <- optimizeExpr e cond
    (benv, b', ob) <- optimizeBodyMax e False b
    (ebenv, eb', oeb) <- optimizeBodyMax e False (fromMaybe [] eb)
    if isStatic cond'
        then if toBool cond'
            then Ok (benv, b', True)
            else Ok (ebenv, eb', True)
        else if allBranchesReturn b'
            then Ok (e, If cond' b' Nothing : eb', ob || oeb || (not . null) eb')
            else Ok (e, [If cond' b' (nothingIfEmpty eb')], ocond || ob || oeb)
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

-- Body

optimizeBody :: Env -> Body -> Result String (Env, Body, Bool)
optimizeBody e [] = Ok (e, [], False)
optimizeBody e (s@(ReturnStatement _):_:_) = do
    (e', sl, _) <- optimizeStatement e s
    return (e', sl, True)
optimizeBody e (s:xs) = do
    (e', sl, so) <- optimizeStatement e s
    (e'', xs', bo) <- optimizeBody e' xs
    return (e'', sl ++ xs', so || bo)

optimizeBodyMax :: Env -> Bool -> Body -> Result String (Env, Body, Bool)
optimizeBodyMax e o b = do
    (e', b', ob) <- optimizeBody e b
    if ob then optimizeBodyMax e' True b' else Ok (e', b', o)

-- Program

optimizeProg :: Program -> Result String Program
optimizeProg (Program b) = do
    (_, b', _) <- optimizeBodyMax env False b
    return (Program b')
