module VM.System.VMUtils
  ( i32BinOp,
    i64BinOp,
    i64BinBoolOp,
    i32Unary,
    i64Unary,
    i64UnaryBool,
    f32BinOp,
    f32BinBoolOp,
    f64BinOp,
    f64BinBoolOp,
    advPC,
    topFrame,
    replaceTopFrame,
    popVal,
    popI32,
    popI64,
    popF32,
    popF64,
  )
where

import Data.Int (Int32, Int64)
import VM.Data.AST (Value (..))
import VM.Data.VMTypes (Frame (frPC), VM (operandStack, vmFrames))

i32BinOp :: VM -> (Int32 -> Int32 -> Int32) -> Either String VM
i32BinOp vm f = do
  (x, vm1) <- popI32 vm
  (y, vm2) <- popI32 vm1
  advPC vm2 (vm2 {operandStack = I32 (f y x) : operandStack vm2})

i64BinOp :: VM -> (Int64 -> Int64 -> Int64) -> Either String VM
i64BinOp vm f = do
  (x, vm1) <- popI64 vm
  (y, vm2) <- popI64 vm1
  advPC vm2 (vm2 {operandStack = I64 (f y x) : operandStack vm2})

i64BinBoolOp :: VM -> (Int64 -> Int64 -> Int32) -> Either String VM
i64BinBoolOp vm f = do
  (x, vm1) <- popI64 vm
  (y, vm2) <- popI64 vm1
  advPC vm2 (vm2 {operandStack = I32 (f y x) : operandStack vm2})

i32Unary :: VM -> (Int32 -> Int32) -> Either String VM
i32Unary vm f = do
  (x, vm1) <- popI32 vm
  advPC vm1 (vm1 {operandStack = I32 (f x) : operandStack vm1})

i64Unary :: VM -> (Int64 -> Int64) -> Either String VM
i64Unary vm f = do
  (x, vm1) <- popI64 vm
  advPC vm1 (vm1 {operandStack = I64 (f x) : operandStack vm1})

i64UnaryBool :: VM -> (Int64 -> Int32) -> Either String VM
i64UnaryBool vm f = do
  (x, vm1) <- popI64 vm
  advPC vm1 (vm1 {operandStack = I32 (f x) : operandStack vm1})

f32BinOp :: VM -> (Float -> Float -> Float) -> Either String VM
f32BinOp vm f = do
  (x, vm1) <- popF32 vm
  (y, vm2) <- popF32 vm1
  advPC vm2 (vm2 {operandStack = F32 (f y x) : operandStack vm2})

f32BinBoolOp :: VM -> (Float -> Float -> Int32) -> Either String VM
f32BinBoolOp vm f = do
  (x, vm1) <- popF32 vm
  (y, vm2) <- popF32 vm1
  advPC vm2 (vm2 {operandStack = I32 (f y x) : operandStack vm2})

f64BinOp :: VM -> (Double -> Double -> Double) -> Either String VM
f64BinOp vm f = do
  (x, vm1) <- popF64 vm
  (y, vm2) <- popF64 vm1
  advPC vm2 (vm2 {operandStack = F64 (f y x) : operandStack vm2})

f64BinBoolOp :: VM -> (Double -> Double -> Int32) -> Either String VM
f64BinBoolOp vm f = do
  (x, vm1) <- popF64 vm
  (y, vm2) <- popF64 vm1
  advPC vm2 (vm2 {operandStack = I32 (f y x) : operandStack vm2})

popI32 :: VM -> Either String (Int32, VM)
popI32 vm = do
  (val, vm1) <- popVal vm
  case val of
    I32 x -> Right (x, vm1)
    _ -> Left "Expected i32 on stack"

popI64 :: VM -> Either String (Int64, VM)
popI64 vm = do
  (val, vm1) <- popVal vm
  case val of
    I64 x -> Right (x, vm1)
    _ -> Left "Expected i64 on stack"

popF32 :: VM -> Either String (Float, VM)
popF32 vm = do
  (val, vm1) <- popVal vm
  case val of
    F32 x -> Right (x, vm1)
    _ -> Left "Expected f32 on stack"

popF64 :: VM -> Either String (Double, VM)
popF64 vm = do
  (val, vm1) <- popVal vm
  case val of
    F64 x -> Right (x, vm1)
    _ -> Left "Expected f64 on stack"

popVal :: VM -> Either String (Value, VM)
popVal vm =
  case operandStack vm of
    (v : vs) -> Right (v, vm {operandStack = vs})
    _ -> Left "popVal: stack underflow"

advPC :: VM -> VM -> Either String VM
advPC _ newVM = do
  f <- topFrame newVM
  let f' = f {frPC = frPC f + 1}
  Right (replaceTopFrame newVM f')

topFrame :: VM -> Either String Frame
topFrame vm =
  case vmFrames vm of
    [] -> Left "No active frame"
    (f : _) -> Right f

replaceTopFrame :: VM -> Frame -> VM
replaceTopFrame vm fr' =
  case vmFrames vm of
    [] -> vm
    (_ : xs) -> vm {vmFrames = fr' : xs}