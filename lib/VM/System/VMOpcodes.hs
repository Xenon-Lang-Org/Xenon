module VM.System.VMOpcodes
  ( executeInstruction,
  )
where

import Data.Bits (complement, shiftL, shiftR, xor, (.&.), (.|.))
import VM.Data.AST
  ( FuncType (ftParamCount, ftResultCount),
    Instruction (..),
    Module (modFuncTypes, modFunctions, modTypes),
    Value (..),
    WasmFunction (funcBody, funcLocals),
  )
import VM.Data.VMTypes
  ( Frame (..),
    Label (Label, labelCond, labelPC, labelType),
    VM (operandStack, vmFrames, vmGlobals, vmModule),
  )
import VM.System.VMUtils
  ( advPC,
    f32BinOp,
    f32BinBoolOp,
    f64BinOp,
    f64BinBoolOp,
    i32BinOp,
    i32Unary,
    i64BinOp,
    i64BinBoolOp,
    i64Unary,
    i64UnaryBool,
    popI32,
    popVal,
    replaceTopFrame,
    topFrame,
  )

executeInstruction :: Instruction -> VM -> Either String VM
executeInstruction instr vm =
  case instr of
    -- CONST
    I32Const n ->
      advPC vm (vm {operandStack = I32 n : operandStack vm})
    I64Const n ->
      advPC vm (vm {operandStack = I64 n : operandStack vm})
    F32Const f ->
      advPC vm (vm {operandStack = F32 f : operandStack vm})
    F64Const f ->
      advPC vm (vm {operandStack = F64 f : operandStack vm})

    -- MATH
    I32Add -> i32BinOp vm (+)
    I64Add -> i64BinOp vm (+)
    F32Add -> f32BinOp vm (+)
    F64Add -> f64BinOp vm (+)

    I32Sub -> i32BinOp vm (-)
    I64Sub -> i64BinOp vm (-)
    F32Sub -> f32BinOp vm (-)
    F64Sub -> f64BinOp vm (-)

    I32Mul -> i32BinOp vm (*)
    I64Mul -> i64BinOp vm (*)
    F32Mul -> f32BinOp vm (*)
    F64Mul -> f64BinOp vm (*)

    I32DivS -> i32BinOp vm div
    I64DivS -> i64BinOp vm div
    F32Div -> f32BinOp vm (/)
    F64Div -> f64BinOp vm (/)

    I32RemS -> i32BinOp vm (\x y -> if y == 0 then 0 else x `rem` y)
    I64RemS -> i64BinOp vm (\x y -> if y == 0 then 0 else x `rem` y)

    I32And -> i32BinOp vm (.&.)
    I64And -> i64BinOp vm (.&.)

    I32Or -> i32BinOp vm (.|.)
    I64Or -> i64BinOp vm (.|.)

    I32Xor -> i32BinOp vm xor
    I64Xor -> i64BinOp vm xor

    I32Not -> i32Unary vm complement
    I64Not -> i64Unary vm complement

    I32Shl -> i32BinOp vm (\x y -> x `shiftL` fromIntegral y)
    I64Shl -> i64BinOp vm (\x y -> x `shiftL` fromIntegral y)

    I32Shr -> i32BinOp vm (\x y -> x `shiftR` fromIntegral y)
    I64Shr -> i64BinOp vm (\x y -> x `shiftR` fromIntegral y)

    I32GtS -> i32BinOp vm (\x y -> if x > y then 1 else 0)
    I64GtS -> i64BinBoolOp vm (\x y -> if x > y then 1 else 0)
    F32Gt -> f32BinBoolOp vm (\x y -> if x > y then 1 else 0)
    F64Gt -> f64BinBoolOp vm (\x y -> if x > y then 1 else 0)

    I32LtS -> i32BinOp vm (\x y -> if x < y then 1 else 0)
    I64LtS -> i64BinBoolOp vm (\x y -> if x < y then 1 else 0)
    F32Lt -> f32BinBoolOp vm (\x y -> if x < y then 1 else 0)
    F64Lt -> f64BinBoolOp vm (\x y -> if x < y then 1 else 0)

    I32Eq -> i32BinOp vm (\x y -> if x == y then 1 else 0)
    I64Eq -> i64BinBoolOp vm (\x y -> if x == y then 1 else 0)
    F32Eq -> f32BinBoolOp vm (\x y -> if x == y then 1 else 0)
    F64Eq -> f64BinBoolOp vm (\x y -> if x == y then 1 else 0)

    I32Ne -> i32BinOp vm (\x y -> if x /= y then 1 else 0)
    I64Ne -> i64BinBoolOp vm (\x y -> if x /= y then 1 else 0)
    F32Ne -> f32BinBoolOp vm (\x y -> if x /= y then 1 else 0)
    F64Ne -> f64BinBoolOp vm (\x y -> if x /= y then 1 else 0)

    I32GeS -> i32BinOp vm (\x y -> if x >= y then 1 else 0)
    I64GeS -> i64BinBoolOp vm (\x y -> if x >= y then 1 else 0)
    F32Ge -> f32BinBoolOp vm (\x y -> if x >= y then 1 else 0)
    F64Ge -> f64BinBoolOp vm (\x y -> if x >= y then 1 else 0)

    I32LeS -> i32BinOp vm (\x y -> if x <= y then 1 else 0)
    I64LeS -> i64BinBoolOp vm (\x y -> if x <= y then 1 else 0)
    F32Le -> f32BinBoolOp vm (\x y -> if x <= y then 1 else 0)
    F64Le -> f64BinBoolOp vm (\x y -> if x <= y then 1 else 0)

    I32Eqz -> i32Unary vm (\x -> if x == 0 then 1 else 0)
    I64Eqz -> i64UnaryBool vm (\x -> if x == 0 then 1 else 0)

    -- MEMORY
    LocalGet idx -> do
      f <- topFrame vm
      if idx < length (frLocals f)
        then
          let v = frLocals f !! idx
           in advPC vm (vm {operandStack = v : operandStack vm})
        else Left $ "local.get: out of range " ++ show idx
    LocalSet idx -> do
      (val, vm1) <- popVal vm
      f <- topFrame vm1
      if idx >= length (frLocals f)
        then Left $ "local.set: out of range " ++ show idx
        else
          let newLocs = take idx (frLocals f) ++ [val] ++ drop (idx + 1) (frLocals f)
              f' = f {frLocals = newLocs}
           in advPC (replaceTopFrame vm1 f') (replaceTopFrame vm1 f')
    GlobalGet gdx ->
      if gdx < length (vmGlobals vm)
        then
          let gVal = vmGlobals vm !! gdx
           in advPC vm (vm {operandStack = gVal : operandStack vm})
        else Left $ "global.get: out of range " ++ show gdx
    GlobalSet gdx -> do
      (val, vm1) <- popVal vm
      if gdx >= length (vmGlobals vm1)
        then Left $ "global.set: out of range " ++ show gdx
        else
          let newG = take gdx (vmGlobals vm1) ++ [val] ++ drop (gdx + 1) (vmGlobals vm1)
           in advPC vm1 (vm1 {vmGlobals = newG})

    -- CONTROL
    Call fn -> do
      let vmCallerPC = incPC vm
      doCall vmCallerPC fn

    If -> do
      (cond, vm1) <- popI32 vm
      let vm2 = incPC vm1
      case vmFrames vm2 of
        [] ->
          Left "If: no matching frame in vm2"
        (fr : frs) ->
          let newLabel =
                Label
                  { labelType = "if",
                    labelPC = frPC fr,
                    labelCond = Just $ fromIntegral cond
                  }
              fr' = fr {frLabels = newLabel : frLabels fr}
              vmL = vm2 {vmFrames = fr' : frs}
           in if cond == 0
                then skipOneBlock vmL 1
                else return vmL

    Else -> do
      let vm2 = incPC vm
      case vmFrames vm2 of
        [] ->
          Left "Else: no matching frame in vm2"
        (fr : _) ->
          case frLabels fr of
            (lbl : _)
              | labelType lbl == "if" ->
                  case labelCond lbl of
                    Just 1 ->
                      skipOneBlock vm2 1
                    Just 0 ->
                      return vm2
                    _ ->
                      Left "Else: missing labelCond in top label"
            _ ->
              Left "Else: no matching 'if' label on stack"

    Block -> pushLabel vm "block"

    Loop -> do
      f <- topFrame vm
      case frLabels f of
        (Label {labelType = "loop", labelPC = pcHere} : _)
          | pcHere == frPC f ->
              advPC vm vm
        _ ->
          pushLabel vm "loop"

    BrIf lbl -> do
      (cond, vm1) <- popI32 vm
      if cond /= 0
        then brJump vm1 lbl
        else advPC vm1 vm1

    Br lbl ->
      brJump vm lbl

    Return -> do
      case vmFrames vm of
        [] ->
          Left "Return: No active frame to return from."
        (currentFrame : callerFrames) -> do
          let retCount = getReturnCount vm (frFuncIndex currentFrame)
          let stack = operandStack vm
          if length stack < retCount
            then Left "Return: Not enough values on the stack for return."
            else do
              let (retVals, newStack) = splitAt retCount stack
              let updatedVM =
                    vm
                      { vmFrames = callerFrames,
                        operandStack = retVals ++ newStack
                      }
              Right updatedVM

    End -> popLabel vm

getReturnCount :: VM -> Int -> Int
getReturnCount vm fnIdx =
  let m = vmModule vm
      typeIndex = modFuncTypes m !! fnIdx
      funcTy = modTypes m !! typeIndex
   in ftResultCount funcTy

getParamCount :: VM -> Int -> Int
getParamCount vm fnIdx =
  let m = vmModule vm
      typeIndex = modFuncTypes m !! fnIdx
      funcTy = modTypes m !! typeIndex
   in ftParamCount funcTy

doCall :: VM -> Int -> Either String VM
doCall vm fnIdx = do
  if fnIdx < 0 || fnIdx >= length (modFunctions (vmModule vm))
    then Left $ "call: function idx out of range " ++ show fnIdx
    else do
      let paramCount = getParamCount vm fnIdx
      let stackBefore = operandStack vm
      if length stackBefore < paramCount
        then Left "Stack underflow for call parameters"
        else do
          let (args, newStack) = splitAt paramCount stackBefore
          let newFrame =
                Frame
                  { frFuncIndex = fnIdx,
                    frLocals = reverse args ++ funcLocals (modFunctions (vmModule vm) !! fnIdx),
                    frPC = 0,
                    frLabels = []
                  }
          Right
            vm
              { operandStack = newStack,
                vmFrames = newFrame : vmFrames vm
              }

pushLabel :: VM -> String -> Either String VM
pushLabel vm labTy = do
  f <- topFrame vm
  let lab = Label {labelType = labTy, labelPC = frPC f, labelCond = Nothing}
      f' = f {frLabels = lab : frLabels f}
  advPC (replaceTopFrame vm f') (replaceTopFrame vm f')

popLabel :: VM -> Either String VM
popLabel vm = do
  f <- topFrame vm
  case frLabels f of
    [] -> advPC vm vm
    (_ : xs) ->
      let f' = f {frLabels = xs}
       in advPC (replaceTopFrame vm f') (replaceTopFrame vm f')

skipUntilEnd :: VM -> Either String VM
skipUntilEnd vm0 = do
  f <- topFrame vm0
  let code = currentFuncBody vm0
      pc = frPC f
  if pc >= length code
    then
      Right vm0
    else case code !! pc of
      End -> do
        popLabel vm0
      _ ->
        skipUntilEnd (incPC vm0)

skipOneBlock :: VM -> Int -> Either String VM
skipOneBlock vm nesting =
  if nesting <= 0
    then Right vm
    else do
      f <- topFrame vm
      let code = currentFuncBody vm
          pc = frPC f
      if pc >= length code
        then Right vm
        else do
          let instr = code !! pc
          case instr of
            Block -> skipOneBlock (incPC vm) (nesting + 1)
            Loop -> skipOneBlock (incPC vm) (nesting + 1)
            If -> skipOneBlock (incPC vm) (nesting + 1)
            Else ->
              if nesting == 1
                then
                  Right vm
                else
                  skipOneBlock (incPC vm) nesting
            End ->
              if nesting == 1
                then
                  skipOneBlock vm (nesting - 1)
                else
                  skipOneBlock (incPC vm) (nesting - 1)
            _ ->
              skipOneBlock (incPC vm) nesting

brJump :: VM -> Int -> Either String VM
brJump vm lbl = do
  f <- topFrame vm
  if lbl < 0 || lbl >= length (frLabels f)
    then Left $ "br: invalid label index " ++ show lbl
    else do
      let lab = frLabels f !! lbl
      if labelType lab == "loop"
        then
          let f' = f {frPC = labelPC lab}
           in Right (replaceTopFrame vm f')
        else do
          let keep = take (lbl + 1) (frLabels f)
              f' = f {frLabels = keep}
          skipUntilEnd (replaceTopFrame vm f')

incPC :: VM -> VM
incPC vm =
  case vmFrames vm of
    [] -> vm
    (f : fs) -> vm {vmFrames = f {frPC = frPC f + 1} : fs}

currentFuncBody :: VM -> [Instruction]
currentFuncBody vm =
  case vmFrames vm of
    [] -> []
    (f : _) ->
      let fn = modFunctions (vmModule vm) !! frFuncIndex f
       in funcBody fn
