module Interpreter.BinOp
    (
        evalBinOp
    )
where

import Parser.Data.Ast

-- Comparaison Binary Operations

boolToIntExpr :: Bool -> Expression
boolToIntExpr False = ELiteral $ IntLiteral 0
boolToIntExpr True = ELiteral $ IntLiteral 1

boolToFloatExpr :: Bool -> Expression
boolToFloatExpr False = ELiteral $ FloatLiteral 0
boolToFloatExpr True = ELiteral $ FloatLiteral 1

doCmpBinOp :: BinOp -> Float -> Float -> Bool
doCmpBinOp Eq l r = l == r
doCmpBinOp Neq l r = l /= r
doCmpBinOp Lt l r = l < r
doCmpBinOp Gt l r = l > r
doCmpBinOp Le l r = l <= r
doCmpBinOp Ge l r = l >= r
doCmpBinOp And l r = (l /= 0) && (r /= 0)
doCmpBinOp Or l r = (l /= 0) || (r /= 0)
doCmpBinOp _ _ _ = False

evalCmpBinOp :: BinOp -> Literal -> Literal -> Expression
evalCmpBinOp op (IntLiteral l) (IntLiteral r) =
    boolToIntExpr $ doCmpBinOp op (fromIntegral l) (fromIntegral r)
evalCmpBinOp op (FloatLiteral l) (IntLiteral r) =
    boolToFloatExpr $ doCmpBinOp op l (fromIntegral r)
evalCmpBinOp op (IntLiteral l) (FloatLiteral r) =
    boolToFloatExpr $ doCmpBinOp op (fromIntegral l) r
evalCmpBinOp op (FloatLiteral l) (FloatLiteral r) =
    boolToFloatExpr $ doCmpBinOp op l r

-- Arithmetic Binary Operations

floatToIntExpr :: Float -> Expression
floatToIntExpr n = ELiteral $ IntLiteral $ round n

floatToFloatExpr :: Float -> Expression
floatToFloatExpr n = ELiteral $ FloatLiteral n

doArBinOp :: BinOp -> Float -> Float -> Float
doArBinOp Add l r = l + r
doArBinOp Sub l r = l - r
doArBinOp Mul l r = l * r
doArBinOp Div l r = l / r
doArBinOp _ _ _ = 0

evalArBinOp :: BinOp -> Literal -> Literal -> Expression
evalArBinOp op (IntLiteral l) (IntLiteral r) =
    floatToIntExpr $ doArBinOp op (fromIntegral l) (fromIntegral r)
evalArBinOp op (FloatLiteral l) (FloatLiteral r) =
    floatToFloatExpr $ doArBinOp op l r
evalArBinOp op (FloatLiteral l) (IntLiteral r) =
    floatToFloatExpr $ doArBinOp op l (fromIntegral r)
evalArBinOp op (IntLiteral l) (FloatLiteral r) =
    floatToFloatExpr $ doArBinOp op (fromIntegral l) r

-- Binary Operation

binOpIsAr :: BinOp -> Bool
binOpIsAr Add = True
binOpIsAr Sub = True
binOpIsAr Mul = True
binOpIsAr Div = True
binOpIsAr _ = False

evalBinOp :: BinOp -> Literal -> Literal -> Expression
evalBinOp op l r | binOpIsAr op = evalArBinOp op l r
                 | otherwise = evalCmpBinOp op l r
