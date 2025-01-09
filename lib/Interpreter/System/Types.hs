module Interpreter.System.Types
    (
        castExpr,
        toLiteralExpr,
        toBool
    )
where

import Utils.Data.Result
import Parser.Data.Ast
import Interpreter.Data.Environment

wrap :: (Num a, Ord a) => (a, a) -> a -> a
wrap (mn, mx) n | n >= mn && n <= mx = n
                   | n > mx = wrap (mn, mx) (mn + (n - mx))
                   | otherwise = wrap (mn, mx) (mx - (n + mn))

castInteger :: Integer -> Primitive -> Integer
castInteger n I8 = wrap (-128, 127) n
castInteger n I16 = wrap (-32768, 32767) n
castInteger n I32 = wrap (-2147483648, 2147483647) n
castInteger n I64 = wrap (-9223372036854775808, 9223372036854775807) n
castInteger n U8 = wrap (0, 255) n
castInteger n U16 = wrap (0, 65535) n
castInteger n U32 = wrap (0, 4294967295) n
castInteger n U64 = wrap (0, 18446744073709551615) n 
castInteger n _ = n

castDouble :: Double -> Primitive -> Double
castDouble n F32 = wrap (-3.4028235e38, 3.4028235e38) n
castDouble n F64 = wrap (-1.7976931348623157e308, 1.7976931348623157e308) n
castDouble n _ = n

isFloat :: Primitive -> Bool
isFloat t = t `elem` [F32, F64]

castLiteral :: Literal -> Primitive -> Literal
castLiteral (IntLiteral n) t = if isFloat t
    then FloatLiteral $ castDouble (fromIntegral n) t
    else IntLiteral $ castInteger n t
castLiteral (FloatLiteral n) t = if isFloat t
    then FloatLiteral $ castDouble n t
    else IntLiteral $ castInteger (round n) t

castExpr :: Env -> Expression -> Type -> Result String Expression
castExpr _ (ELiteral l) (PrimitiveType t) = Ok $ ELiteral $ castLiteral l t
castExpr _ _ t = Err $ "Failed to cast expression to " ++ show t

toLiteralExpr :: Type -> Expression -> Result String Literal
toLiteralExpr _ (ELiteral v) = Ok v
toLiteralExpr _ v = Err $ "Cannot deduce literal from " ++ show v

numToBool :: (Num a, Eq a) => a -> Bool
numToBool 0 = False
numToBool _ = True

toBool :: Expression -> Result String Bool
toBool (ELiteral (IntLiteral n)) = Ok $ numToBool n
toBool (ELiteral (FloatLiteral n)) = Ok $ numToBool n
toBool _ = Err "Invalid boolean expression conversion"