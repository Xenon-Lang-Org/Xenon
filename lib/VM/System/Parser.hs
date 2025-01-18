module VM.System.Parser
  ( parseWasm,
  )
where

import Control.Monad (replicateM)
import Data.Binary.Get
  ( Get,
    bytesRead,
    getByteString,
    getWord32le,
    getWord8,
    isEmpty,
    lookAhead,
    runGetOrFail,
    skip,
  )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.Word (Word32)
import VM.Data.AST
  ( Export (Export),
    FuncType (FuncType),
    Global (..),
    Instruction (End),
    Module (..),
    ValType (..),
    Value (..),
    WasmFunction (WasmFunction),
  )
import VM.System.ParserOpcodes (parseInstruction)
import VM.System.ParserUtils (getS32LEB, getU32LEB)

parseWasm :: BS.ByteString -> Either String Module
parseWasm bs =
  case runGetOrFail getModule (BL.fromStrict bs) of
    Left (_, _, err) -> Left err
    Right (_, _, m) -> Right m

getModule :: Get Module
getModule = do
  magicBytes <- getByteString 4
  if magicBytes /= BS.pack [0x00, 0x61, 0x73, 0x6d]
    then fail $ "Invalid WASM magic: got " ++ show magicBytes
    else do
      version <- getWord32le
      if version /= 0x00000001
        then fail $ "Invalid WASM version: " ++ show version
        else do
          let emptyMod = Module [] [] [] [] []
          parseSections emptyMod

parseSections :: Module -> Get Module
parseSections mod0 = do
  done <- isEmpty
  if done
    then return mod0
    else do
      sid <- getWord8
      sz <- getU32LEB
      case sid of
        1 -> do
          m' <- parseTypeSection mod0 sz
          parseSections m'
        2 -> skip (fromIntegral sz) >> parseSections mod0
        3 -> do
          m' <- parseFuncSection mod0 sz
          parseSections m'
        4 -> skip (fromIntegral sz) >> parseSections mod0
        5 -> skip (fromIntegral sz) >> parseSections mod0
        6 -> do
          m' <- parseGlobalSection mod0 sz
          parseSections m'
        7 -> do
          m' <- parseExportSection mod0 sz
          parseSections m'
        8 -> skip (fromIntegral sz) >> parseSections mod0
        9 -> skip (fromIntegral sz) >> parseSections mod0
        10 -> do
          m' <- parseCodeSection mod0 sz
          parseSections m'
        11 -> skip (fromIntegral sz) >> parseSections mod0
        _ -> skip (fromIntegral sz) >> parseSections mod0

parseTypeSection :: Module -> Word32 -> Get Module
parseTypeSection mod0 secSize = do
  startPos <- bytesRead
  typeCount <- getU32LEB
  ftList <- replicateM (fromIntegral typeCount) parseOneFuncType
  endPos <- bytesRead
  skip (fromIntegral secSize - fromIntegral (endPos - startPos))
  let mod1 = mod0 {modTypes = ftList}
  return mod1

parseOneFuncType :: Get FuncType
parseOneFuncType = do
  form <- getWord8
  if form /= 0x60
    then fail "Expected form=0x60 for func type"
    else do
      pCount <- getU32LEB
      paramTypes <- replicateM (fromIntegral pCount) getValType
      rCount <- getU32LEB
      returnTypes <- replicateM (fromIntegral rCount) getValType
      return $ FuncType (fromIntegral pCount) paramTypes (fromIntegral rCount) returnTypes

parseFuncSection :: Module -> Word32 -> Get Module
parseFuncSection mod0 secSize = do
  startPos <- bytesRead
  fCount <- getU32LEB
  idxs <- replicateM (fromIntegral fCount) getU32LEB
  endPos <- bytesRead
  skip (fromIntegral secSize - fromIntegral (endPos - startPos))
  let more = map fromIntegral idxs
  let mod1 = mod0 {modFuncTypes = modFuncTypes mod0 ++ more}
  return mod1

parseExportSection :: Module -> Word32 -> Get Module
parseExportSection mod0 secSize = do
  startPos <- bytesRead
  expCount <- getU32LEB
  exps <- replicateM (fromIntegral expCount) parseOneExport
  endPos <- bytesRead
  skip (fromIntegral secSize - fromIntegral (endPos - startPos))
  let mod1 = mod0 {modExports = modExports mod0 ++ exps}
  return mod1

parseOneExport :: Get Export
parseOneExport = do
  nmLen <- getU32LEB
  nmBs <- getByteString (fromIntegral nmLen)
  _ <- getWord8
  Export (byteStringToString nmBs) . fromIntegral <$> getU32LEB

byteStringToString :: BS.ByteString -> String
byteStringToString = map (toEnum . fromEnum) . BS.unpack

parseCodeSection :: Module -> Word32 -> Get Module
parseCodeSection mod0 secSize = do
  startPos <- bytesRead
  fCount <- getU32LEB
  fs <- replicateM (fromIntegral fCount) parseOneFunction
  endPos <- bytesRead
  skip (fromIntegral secSize - fromIntegral (endPos - startPos))
  let mod1 = mod0 {modFunctions = modFunctions mod0 ++ fs}
  return mod1

parseOneFunction :: Get WasmFunction
parseOneFunction = do
  bodySize <- getU32LEB
  funcBodyStart <- bytesRead
  nLocalDecls <- getU32LEB
  locs <- parseLocals nLocalDecls
  instrs <- parseInstructions funcBodyStart (fromIntegral bodySize)
  endPos <- bytesRead
  let consumedEnd = endPos - funcBodyStart
  let leftover = fromIntegral bodySize - consumedEnd
  skip $ fromIntegral leftover
  return $ WasmFunction locs instrs

parseLocals :: Word32 -> Get [Value]
parseLocals 0 = return []
parseLocals n = do
  cnt <- getU32LEB
  vtp <- getWord8
  let initVal = case vtp of
        0x7F -> I32 0
        0x7E -> I64 0
        0x7D -> F32 0
        0x7C -> F64 0
        _ -> I32 0
  rest <- parseLocals (n - 1)
  return (replicate (fromIntegral cnt) initVal ++ rest)

parseInstructions :: Int64 -> Int64 -> Get [Instruction]
parseInstructions funcBodyStart bodySize = do
  currentPos <- bytesRead
  let consumed = currentPos - funcBodyStart
  if consumed >= bodySize
    then
      pure []
    else do
      op <- lookAhead getWord8
      if op == 0x0B
        then do
          _ <- getWord8
          rest <- parseInstructions funcBodyStart bodySize
          pure (End : rest)
        else do
          _ <- getWord8
          instr <- parseInstruction op
          rest <- parseInstructions funcBodyStart bodySize
          pure (instr : rest)

parseGlobalSection :: Module -> Word32 -> Get Module
parseGlobalSection mod0 secSize = do
  startPos <- bytesRead
  gCount <- getU32LEB
  globals <- replicateM (fromIntegral gCount) parseOneGlobal
  endPos <- bytesRead
  skip (fromIntegral secSize - fromIntegral (endPos - startPos))
  let mod1 = mod0 {modGlobals = modGlobals mod0 ++ globals}
  return mod1

parseOneGlobal :: Get Global
parseOneGlobal = do
  valType <- getValType
  mutFlag <- getWord8
  initVal <- parseInitExpr
  return $
    Global
      { globalType = valType,
        globalMut = mutFlag /= 0,
        globalInit = initVal
      }

getValType :: Get ValType
getValType = do
  vt <- getWord8
  return $ case vt of
    0x7F -> V_I32
    0x7E -> V_I64
    0x7D -> V_F32
    0x7C -> V_F64
    _ -> error $ "Unsupported ValType: " ++ show vt

parseInitExpr :: Get Value
parseInitExpr = do
  instr <- getWord8
  case instr of
    0x41 -> do
      n <- getS32LEB
      expectEnd
      return $ I32 n
    _ -> fail $ "Unsupported global init expression opcode: " ++ show instr

expectEnd :: Get ()
expectEnd = do
  endOp <- getWord8
  if endOp == 0x0B
    then return ()
    else fail "Expected End opcode in init expression"