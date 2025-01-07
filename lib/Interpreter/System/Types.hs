module Interpreter.System.Types
    (
        -- castExpr,
        -- toLiteralExpr
    )
where

-- import Utils.Data.Result
-- import Parser.Data.Ast
-- import Interpreter.Environment

-- wrap :: (Int, Int) -> Int -> Int
-- wrap (mn, mx) n | n >= mn && n <= mx = n
--                    | n > mx = wrap (mn, mx) (mn + (n - mx))
--                    | otherwise = wrap (mn, mx) (mx - (n + mn))

-- castInt :: Int -> Primitive -> Int
-- castInt n I8 = wrap (-128, 127) n
-- castInt n I16 = wrap (-32768, 32767) n
-- castInt n I32 = wrap (-2147483648, 2147483647) n
-- castInt n I64 = wrap (-9223372036854775808, 9223372036854775807) n
-- castInt n U8 = wrap (0, 255) n
-- castInt n U16 = wrap (0, 65535) n
-- castInt n U32 = wrap (0, 4294967295) n
-- -- castInt n U64 = wrap (0, 18446744073709551615) n -- Parsing uses Int so U64 is not possible 
-- castInt n _ = n

-- castFloat :: Float -> Primitive -> Float
-- castFloat n _ = n -- Parsing uses Float so F64 is not possible

-- castLiteral :: Literal -> Primitive -> Literal
-- castLiteral (IntLiteral n) t
--     | t `elem` [F32, F64] = FloatLiteral $ castFloat (fromIntegral n) t
--     | otherwise = IntLiteral $ castInt n t
-- castLiteral (FloatLiteral n) t
--     | t `elem` [F32, F64] = FloatLiteral $ castFloat n t
--     | otherwise = IntLiteral $ castInt (round n) t

-- castExpr :: Env -> Expression -> Type -> Result String Expression
-- castExpr _ (ELiteral l) (PrimitiveType t) = Ok $ ELiteral $ castLiteral l t
-- castExpr _ _ t = Err $ "Failed to cast expression to " ++ show t

-- toLiteralExpr :: Type -> Expression -> Result String Literal
-- toLiteralExpr _ (ELiteral v) = Ok v
-- toLiteralExpr _ v = Err $ "Cannot deduce literal from " ++ show v
