module Interpreter.System.Types
    (
        castExpr,
        toLiteralExpr,
        toBool,
    )
where

import Utils.Data.Result
import Parser.Data.Ast
import Interpreter.Data.Environment
import Data.Ord (clamp)

castInteger :: Primitive -> Integer -> Integer
castInteger I8 = clamp (-128, 127)
castInteger I16 = clamp (-32768, 32767)
castInteger I32 = clamp (-2147483648, 2147483647)
castInteger I64 = clamp (-9223372036854775808, 9223372036854775807)
castInteger U8 = clamp (0, 255)
castInteger U16 = clamp (0, 65535)
castInteger U32 = clamp (0, 4294967295)
castInteger U64 = clamp (0, 18446744073709551615) 
castInteger _ = id

castDouble :: Primitive -> Double -> Double
castDouble F32 = clamp (-3.4028235e38, 3.4028235e38)
castDouble F64 = clamp (-1.7976931348623157e308, 1.7976931348623157e308)
castDouble _ = id

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
castExpr _ _ t = Err $ "Failed to cast expression to " ++ show t

toLiteralExpr :: Type -> Expression -> Result String Literal
toLiteralExpr _ (ELiteral v) = Ok v
toLiteralExpr _ v = Err $ "Cannot deduce literal from " ++ show v

toBool :: Expression -> Bool
toBool (ELiteral (IntLiteral 0)) = False
toBool (ELiteral (FloatLiteral 0)) = False
toBool _ = True