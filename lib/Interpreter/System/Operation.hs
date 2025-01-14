module Interpreter.System.Operation
    (
        evalBinOp,
        evalUnaryOp
    )
where

import Parser.Data.Ast
import Utils.Data.Result
import Data.Bits((.&.), (.|.), (.^.), shiftL, shiftR, Bits, complement)
import Control.Applicative

class (Num a, Ord a) => Numeric a
class (Numeric a, Integral a, Bits a) => NumBits a
class (Numeric a, Fractional a) => NumFloat a

instance Numeric Double
instance NumFloat Double
instance Numeric Integer
instance NumBits Integer

-- Tools

toNum :: Numeric a => Bool -> a
toNum True = 1
toNum False = 0

toBool :: Numeric a => a -> Bool
toBool 0 = False
toBool _ = True

toNumFloat :: NumFloat a => Expression -> Result String a
toNumFloat (ELiteral (FloatLiteral n)) = Ok $ realToFrac n 
toNumFloat (ELiteral (IntLiteral n)) = Ok $ fromIntegral n
toNumFloat _ = Err "Invalid binary operator argument"

toNumBits :: NumBits a => Expression -> Result String a
toNumBits (ELiteral (IntLiteral n)) = Ok $ fromIntegral n
toNumBits n = Err ("Invalid binary operator argument " ++ show n)

-- Binary Operations

cmpOp :: Numeric a => (a -> a -> Bool) -> a -> a -> a
cmpOp f l r = toNum (f l r)

boolOp :: Numeric a => (Bool -> Bool -> Bool) -> a -> a -> a
boolOp f l r = toNum (f (toBool l) (toBool r))

evalBin :: Numeric a => BinOp -> a -> a -> a
evalBin op l r = case op of
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

evalBinFloat :: NumFloat a => BinOp -> a -> a -> a
evalBinFloat op l r = case op of
    Div -> l / r
    _ -> evalBin op l r

evalBinBits :: NumBits a => BinOp -> a -> a -> a
evalBinBits op l r = case op of
    Mod -> l `mod` r
    BitAnd -> (.&.) l r
    BitOr -> (.|.) l r
    BitXor -> (.^.) l r
    Shl -> l `shiftL` fromIntegral r
    Shr -> l `shiftR` fromIntegral r
    _ -> evalBin op l r

floatBinEval :: BinOp -> Expression -> Expression -> Result String Expression
floatBinEval Div _ (ELiteral (IntLiteral 0)) = Err "Division by zero" 
floatBinEval Div _ (ELiteral (FloatLiteral 0)) = Err "Division by zero" 
floatBinEval op l r = case mapBoth toNumFloat (l, r) of
    Ok (l', r') -> Ok $ ELiteral $ FloatLiteral $ evalBinFloat op l' r'
    Err msg -> Err msg

bitsBinEval :: BinOp -> Expression -> Expression -> Result String Expression
bitsBinEval op l r = case mapBoth toNumBits (l, r) of
    Ok (l', r') -> Ok $ ELiteral $ IntLiteral $ evalBinBits op l' r'
    Err msg -> Err msg

evalBinOp :: BinOp -> Expression -> Expression -> Result String Expression
evalBinOp Div l r = floatBinEval Div l r
evalBinOp op l r = bitsBinEval op l r <|> floatBinEval op l r

-- Unary Operations

evalUnary :: Numeric a => UnaryOp -> a -> a
evalUnary Negate = toNum . not . toBool
evalUnary Negative = (0 -)
evalUnary _ = id

evalUnaryBits :: NumBits a => UnaryOp -> a -> a
evalUnaryBits BitNot = complement
evalUnaryBits op = evalUnary op

evalUnaryFloat :: NumFloat a => UnaryOp -> a -> a
evalUnaryFloat = evalUnary

bitsUnaryEval :: UnaryOp -> Expression -> Result String Expression
bitsUnaryEval op ex = case toNumBits ex of
    Ok ex' -> Ok $ ELiteral $ IntLiteral $ evalUnaryBits op ex'
    Err m -> Err m

floatUnaryEval :: UnaryOp -> Expression -> Result String Expression
floatUnaryEval op ex = case toNumFloat ex of
    Ok ex' -> Ok $ ELiteral $ FloatLiteral $ evalUnaryFloat op ex'
    Err m -> Err m

evalUnaryOp :: UnaryOp -> Expression -> Result String Expression
evalUnaryOp op ex = bitsUnaryEval op ex <|> floatUnaryEval op ex