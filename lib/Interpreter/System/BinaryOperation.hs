module Interpreter.System.BinaryOperation
    (
        evalBinOp
    )
where

import Parser.Data.Ast
import Utils.Data.Result
import Data.Bits((.&.), (.|.), (.^.), shiftL, shiftR, Bits)
import Control.Applicative

class (Num a, Ord a) => Numeric a
class (Numeric a, Integral a, Bits a) => NumBits a
class (Numeric a, Fractional a) => NumFloat a

instance Numeric Double
instance NumFloat Double
instance Numeric Integer
instance NumBits Integer

-- Other

toNum :: Numeric a => Bool -> a
toNum True = 1
toNum False = 0

toBool :: Numeric a => a -> Bool
toBool 0 = False
toBool _ = True

cmpOp :: Numeric a => (a -> a -> Bool) -> a -> a -> a
cmpOp f l r = toNum (f l r)

boolOp :: Numeric a => (Bool -> Bool -> Bool) -> a -> a -> a
boolOp f l r = toNum (f (toBool l) (toBool r))

evalOp :: Numeric a => BinOp -> a -> a -> a
evalOp op l r = case op of
    Add -> l + r
    Sub -> l - r
    Mul -> l * r
    Eq ->  cmpOp (==) l r
    Neq -> cmpOp (/=) l r
    Lt -> cmpOp (<) l r
    Gt -> cmpOp (>) l r
    Le -> cmpOp (<=) l r
    Ge -> cmpOp (>=) l r
    And -> boolOp (&&) l r
    Or -> boolOp (||) l r
    _ -> l

evalOpFloat :: NumFloat a => BinOp -> a -> a -> a
evalOpFloat op l r = case op of
    Div -> l / r
    _ -> evalOp op l r

evalOpBits :: NumBits a => BinOp -> a -> a -> a
evalOpBits op l r = case op of
    Mod -> l `mod` r
    BitAnd -> (.&.) l r
    BitOr -> (.|.) l r
    BitXor -> (.^.) l r
    Shl -> l `shiftL` fromIntegral r
    Shr -> l `shiftR` fromIntegral r
    _ -> evalOp op l r

toNumFloat :: NumFloat a => Expression -> Result String a
toNumFloat (ELiteral (FloatLiteral n)) = Ok $ realToFrac n 
toNumFloat (ELiteral (IntLiteral n)) = Ok $ fromIntegral n
toNumFloat _ = Err "Invalid binary operator argument"

toNumBits :: NumBits a => Expression -> Result String a
toNumBits (ELiteral (IntLiteral n)) = Ok $ fromIntegral n
toNumBits n = Err ("Invalid binary operator argument " ++ show n)

floatEval :: BinOp -> Expression -> Expression -> Result String Expression
floatEval Div _ (ELiteral (IntLiteral 0)) = Err "Division by zero" 
floatEval Div _ (ELiteral (FloatLiteral 0)) = Err "Division by zero" 
floatEval op l r = case mapBoth toNumFloat (l, r) of
    Ok (l', r') -> Ok $ ELiteral $ FloatLiteral $ evalOpFloat op l' r'
    Err msg -> Err msg

bitsEval :: BinOp -> Expression -> Expression -> Result String Expression
bitsEval op l r = case mapBoth toNumBits (l, r) of
    Ok (l', r') -> Ok $ ELiteral $ IntLiteral $ evalOpBits op l' r'
    Err msg -> Err msg

evalBinOp :: BinOp -> Expression -> Expression -> Result String Expression
evalBinOp Div l r = floatEval Div l r
evalBinOp op l r = bitsEval op l r <|> floatEval op l r
