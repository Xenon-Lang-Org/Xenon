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

wrap :: (Num a, Ord a) => (a, a) -> a -> a
wrap (mn, mx) n | n >= mn && n <= mx = n
                | n > mx = wrap (mn, mx) (mn + (n - mx - 1))
                | otherwise = wrap (mn, mx) (mx - (n + mn + 1))

castInteger :: Primitive -> Integer -> Integer
castInteger I8 = wrap (-128, 127)
castInteger I16 = wrap (-32768, 32767)
castInteger I32 = wrap (-2147483648, 2147483647)
castInteger I64 = wrap (-9223372036854775808, 9223372036854775807)
castInteger U8 = wrap (0, 255)
castInteger U16 = wrap (0, 65535)
castInteger U32 = wrap (0, 4294967295)
castInteger U64 = wrap (0, 18446744073709551615) 
castInteger _ = id

castDouble :: Double -> Primitive -> Double
castDouble n F32 = wrap (-3.4028235e38, 3.4028235e38) n
castDouble n F64 = wrap (-1.7976931348623157e308, 1.7976931348623157e308) n
castDouble n _ = n

isFloat :: Primitive -> Bool
isFloat t = t `elem` [F32, F64]

castLiteral :: Literal -> Primitive -> Literal
castLiteral (IntLiteral n) t = if isFloat t
    then FloatLiteral $ castDouble (fromIntegral n) t
    else IntLiteral $ castInteger t n
castLiteral (FloatLiteral n) t = if isFloat t
    then FloatLiteral $ castDouble n t
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