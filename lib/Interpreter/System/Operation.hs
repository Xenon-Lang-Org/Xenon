module Interpreter.System.Operation
    (
        evalBinOp,
        evalUnaryOp,
        evalSizedBinOp
    )
where

import Parser.Data.Ast
import Utils.Data.Result
import Data.Bits((.&.), (.|.), (.^.), shiftL, shiftR, Bits, complement)
import Control.Applicative

class (Num a, Ord a) => Numeric a
class (Numeric a, Integral a, Bits a) => NumBits a
class (RealFrac a, Numeric a, Fractional a) => NumFloat a

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

roundNumFloat :: NumFloat a => a -> a
roundNumFloat n = integral (round (n * factor)) / factor
    where
        factor = 1000000000000
        integral :: NumFloat a => Integer -> a
        integral = fromIntegral

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

evalBinBits :: NumBits a => Int -> BinOp -> a -> a -> a
evalBinBits s op l r = case op of
    Div -> l `div` r
    Mod -> l `rem` r
    BitAnd -> (.&.) l r
    BitOr -> (.|.) l r
    BitXor -> (.^.) l r
    Shl -> if s == 0 
        then l `shiftL` fromIntegral r
        else l `shiftL` fromIntegral (r .&. fromIntegral (s - 1))
    Shr -> if s == 0 
        then l `shiftR` fromIntegral r
        else l `shiftR` fromIntegral (r .&. fromIntegral (s - 1))
    _ -> evalBin op l r

floatBinEval :: BinOp -> Expression -> Expression -> Result String Expression
floatBinEval Div _ (ELiteral (IntLiteral 0)) = Err "Division by zero"
floatBinEval Div _ (ELiteral (FloatLiteral 0)) = Err "Division by zero"
floatBinEval op l r = case mapBoth toNumFloat (l, r) of
    Ok (l', r') -> Ok $ ELiteral $ FloatLiteral $ roundNumFloat $ evalBinFloat op l' r'
    Err msg -> Err msg

bitsBinEval :: Int -> BinOp -> Expression -> Expression -> Result String Expression
bitsBinEval s op l r = case mapBoth toNumBits (l, r) of
    Ok (l', r') -> Ok $ ELiteral $ IntLiteral $ evalBinBits s op l' r'
    Err msg -> Err msg

evalBinOp :: BinOp -> Expression -> Expression -> Result String Expression
evalBinOp op l r = bitsBinEval 0 op l r <|> floatBinEval op l r

evalSizedBinOp :: Int -> BinOp -> Expression -> Expression -> Result String Expression
evalSizedBinOp s op l r = bitsBinEval s op l r <|> floatBinEval op l r

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
    Ok ex' -> Ok $ ELiteral $ FloatLiteral $ roundNumFloat $ evalUnaryFloat op ex'
    Err m -> Err m

evalUnaryOp :: UnaryOp -> Expression -> Result String Expression
evalUnaryOp op ex = bitsUnaryEval op ex <|> floatUnaryEval op ex