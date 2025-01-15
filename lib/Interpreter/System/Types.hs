module Interpreter.System.Types
    (
        castExpr,
        toBool,
        defaultExpr
    )
where

import Utils.Data.Result
import Parser.Data.Ast
import Interpreter.Data.Environment

wrapSigned :: Integer -> Integer -> Integer
wrapSigned n s = (n `mod` s) - (if n `mod` s >= (s `div` 2) then s else 0)

castInteger :: Primitive -> Integer -> Integer
castInteger I8 n = n `wrapSigned` 256
castInteger I16 n = n `wrapSigned` 65536
castInteger I32 n = n `wrapSigned` 4294967296
castInteger I64 n = n `wrapSigned` 18446744073709551616
castInteger U8 n = n `mod` 256
castInteger U16 n = n `mod` 65536
castInteger U32 n = n `mod` 4294967296
castInteger U64 n = n `mod` 18446744073709551616
castInteger _ n = n

castDouble :: Primitive -> Double -> Double
castDouble F32 n = realToFrac (realToFrac n :: Float)
castDouble _ n = n

isFloat :: Primitive -> Bool
isFloat t = t `elem` [F32, F64]

castLiteral :: Literal -> Primitive -> Literal
castLiteral (IntLiteral n) t = if isFloat t
    then FloatLiteral $ castDouble t (fromIntegral n)
    else IntLiteral $ castInteger t n
castLiteral (FloatLiteral n) t = if isFloat t
    then FloatLiteral $ castDouble t n
    else IntLiteral $ castInteger t (round n)

castExpr :: Env -> Expression -> Type -> Result String Expression
castExpr _ (ELiteral l) (PrimitiveType _ t) = Ok $ ELiteral $ castLiteral l t
castExpr e ex (CustomType mt n) = case deduceType e (CustomType mt n) of
    Ok t' -> castExpr e ex t'
    Err m -> Err m
castExpr _ _ t = Err $ "Failed to cast expression to " ++ show t

toBool :: Expression -> Bool
toBool (ELiteral (IntLiteral 0)) = False
toBool (ELiteral (FloatLiteral 0)) = False
toBool _ = True

deduceType :: Env -> Type -> Result String Type
deduceType e (CustomType _ n) = case fromEnv e n of
    Ok (EType _ t) -> deduceType e t
    Ok _ -> Err $ n ++ " is not a type"
    Err m -> Err m
deduceType _ t = Ok t

defaultExpr :: Env -> Type -> Result String Expression
defaultExpr e = castExpr e (ELiteral $ IntLiteral 0)
