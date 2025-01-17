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
import Data.Ord (clamp) 

clampInt :: Primitive -> Integer -> Integer
clampInt I8 = clamp (-128, 127)
clampInt U8 = clamp (0, 255)
clampInt I16 = clamp (-32768, 32767)
clampInt U16 = clamp (0, 65535)
clampInt I32 = clamp (-2147483648, 2147483647)
clampInt U32 = clamp (0, 4294967295)
clampInt I64 = clamp (-9223372036854775808, 9223372036854775807)
clampInt U64 = clamp (0, 18446744073709551615)
clampInt _ = id

clampFloat :: Primitive -> Double -> Double
clampFloat F32 n = realToFrac (realToFrac n :: Float)
clampFloat _ n = n

isFloat :: Primitive -> Bool
isFloat t = t `elem` [F32, F64]

castLiteral :: Literal -> Primitive -> Literal
castLiteral (IntLiteral n) t = if isFloat t
    then FloatLiteral $ clampFloat t (fromIntegral n)
    else IntLiteral $ clampInt t n
castLiteral (FloatLiteral n) t = if isFloat t
    then FloatLiteral $ clampFloat t n
    else IntLiteral $ clampInt t (round n)

castExpr :: Env -> Expression -> Type -> Result String Expression
castExpr _ (ELiteral l) (PrimitiveType _ t) = Ok $ ELiteral $ castLiteral l t
castExpr e ex (CustomType mt n) = case deduceType e (CustomType mt n) of
    Ok t' -> castExpr e ex t'
    Err m -> Err m
castExpr _ ex t = Err $ "Failed to cast " ++ show ex ++ " to " ++ show t

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
