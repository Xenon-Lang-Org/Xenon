module VM.System.VM
  ( VM (..),
    invokeByNameWithArgs,
  )
where

import Data.List (find)
import VM.Data.AST
  ( Export (exportIndex, exportName),
    FuncType (ftResultCount),
    Global (Global),
    Module
      ( modExports,
        modFuncTypes,
        modFunctions,
        modGlobals,
        modTypes
      ),
    ValType (V_F32, V_F64, V_I32, V_I64),
    Value (..),
    WasmFunction (funcBody, funcLocals),
  )
import VM.Data.VMTypes
  ( Frame (Frame, frFuncIndex, frLabels, frLocals, frPC),
    VM (..),
  )
import VM.System.VMOpcodes (executeInstruction)

invokeByNameWithArgs :: Module -> String -> [Value] -> Either String VM
invokeByNameWithArgs modAST funcName rawArgs = do
  let mExport = find (\e -> exportName e == funcName) (modExports modAST)
  case mExport of
    Nothing ->
      Left $ "No exported function named " ++ show funcName
    Just expRec ->
      let fnIndex = exportIndex expRec
       in callExportedFunction modAST fnIndex rawArgs

callExportedFunction :: Module -> Int -> [Value] -> Either String VM
callExportedFunction modAST fnIndex paramVals = do
  if fnIndex < 0 || fnIndex >= length (modFunctions modAST)
    then Left $ "Function index out of range: " ++ show fnIndex
    else do
      initialGlobals <- mapM evaluateGlobal (modGlobals modAST)
      let initialVM =
            VM
              { vmModule = modAST,
                vmFrames = [],
                operandStack = [],
                vmMemory = replicate (64 * 1024) 0,
                vmGlobals = initialGlobals,
                vmTable = []
              }
      let newFrame =
            Frame
              { frFuncIndex = fnIndex,
                frLocals = paramVals ++ funcLocals (modFunctions modAST !! fnIndex),
                frPC = 0,
                frLabels = []
              }
      let vm1 = initialVM {vmFrames = [newFrame]}
      runLoop vm1

evaluateGlobal :: Global -> Either String Value
evaluateGlobal (Global V_I32 _ (I32 n)) = Right (I32 n)
evaluateGlobal (Global V_I64 _ (I64 n)) = Right (I64 n)
evaluateGlobal (Global V_F32 _ (F32 f)) = Right (F32 f)
evaluateGlobal (Global V_F64 _ (F64 d)) = Right (F64 d)
evaluateGlobal _ = Left "Unsupported global initialization expression"

runLoop :: VM -> Either String VM
runLoop vm =
  case vmFrames vm of
    [] -> Right vm
    (fr : _) ->
      let fnBody = funcBody (modFunctions (vmModule vm) !! frFuncIndex fr)
          pc = frPC fr
       in if pc < 0 || pc >= length fnBody
            then do
              vmPop <- popFrame vm
              runLoop vmPop
            else do
              let instr = fnBody !! pc
              case executeInstruction instr vm of
                Left err -> Left err
                Right vmNext ->
                  if null (vmFrames vmNext)
                    then do
                      Right vmNext
                    else do
                      runLoop vmNext

popFrame :: VM -> Either String VM
popFrame vm =
  case vmFrames vm of
    [] ->
      Right vm
    (callee : rest) -> do
      let retCount = getReturnCount vm $ frFuncIndex callee
      let fullStack = operandStack vm
      if length fullStack < retCount
        then Left "Stack underflow while returning from callee"
        else do
          let (returnVals, newStack) = splitAt retCount fullStack
          case rest of
            [] ->
              Right
                vm
                  { operandStack = returnVals ++ newStack,
                    vmFrames = []
                  }
            (caller : others) ->
              Right
                vm
                  { operandStack = returnVals ++ newStack,
                    vmFrames = caller : others
                  }

getReturnCount :: VM -> Int -> Int
getReturnCount vm fnIdx =
  let m = vmModule vm
      typeIndex = modFuncTypes m !! fnIdx
      fType = modTypes m !! typeIndex
   in ftResultCount fType