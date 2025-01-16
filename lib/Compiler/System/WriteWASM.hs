module Compiler.System.WriteWASM (writeWasmModule) where
import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Compiler.Data.ModuleData
import Compiler.System.EncodeNumbers

encodeVector :: [Word8] -> [Word8]
encodeVector xs =
  let len = encodeU32LEB128 (length xs)
  in len ++ xs

encodeVectorOfBlocks :: [[Word8]] -> [Word8]
encodeVectorOfBlocks blocks =
  let len = encodeU32LEB128 (length blocks)
  in len ++ concat blocks

encodeValueType :: ValueType -> Word8
encodeValueType vt = case vt of
  ModuleI32 -> 0x7F
  ModuleI64 -> 0x7E
  ModuleF32 -> 0x7D
  ModuleF64 -> 0x7C

encodeInstruction :: Instruction -> [Word8]
encodeInstruction instr = case instr of

  ModuleConstI32 n -> [0x41] <> encodeS32LEB128 n   -- i32.const
  ModuleConstI64 n -> [0x42] <> encodeS32LEB128 n   -- i64.const

  ModuleConstF32 f ->
    0x43 : encodeF32 f    -- f32.const
  ModuleConstF64 d ->
    0x44 : encodeF64 d    -- f64.const

  ModuleAdd vt -> case vt of
    ModuleI32 -> [0x6A]  -- i32.add
    ModuleI64 -> [0x7C]  -- i64.add
    ModuleF32 -> [0x92]  -- f32.add
    ModuleF64 -> [0xA0]  -- f64.add

  ModuleSub vt -> case vt of
    ModuleI32 -> [0x6B]  -- i32.sub
    ModuleI64 -> [0x7D]
    ModuleF32 -> [0x93]
    ModuleF64 -> [0xA1]

  ModuleMul vt -> case vt of
    ModuleI32 -> [0x6C]  -- i32.mul
    ModuleI64 -> [0x7E]
    ModuleF32 -> [0x94]
    ModuleF64 -> [0xA2]

  ModuleDiv vt -> case vt of
    ModuleI32 -> [0x6D]  -- i32.div_s
    ModuleI64 -> [0x7F]  -- i64.div_s
    ModuleF32 -> [0x95]  -- f32.div
    ModuleF64 -> [0xA3]  -- f64.div
  
  ModuleMod vt -> case vt of
    ModuleI32 -> [0x6F]  -- i32.rem_s
    ModuleI64 -> [0x81]  -- i64.rem_s
    ModuleF32 -> error "f32.mod not supported"
    ModuleF64 -> error "f64.mod not supported"

  ModuleBitAnd vt -> case vt of
    ModuleI32 -> [0x71]  -- i32.and
    ModuleI64 -> [0x83]  -- i64.and
    ModuleF32 -> error "f32.and not supported"
    ModuleF64 -> error "f64.and not supported"

  ModuleBitOr vt -> case vt of
    ModuleI32 -> [0x72]  -- i32.or
    ModuleI64 -> [0x84]  -- i64.or
    ModuleF32 -> error "f32.or not supported"
    ModuleF64 -> error "f64.or not supported"
  
  ModuleBitXor vt -> case vt of
    ModuleI32 -> [0x73]  -- i32.xor
    ModuleI64 -> [0x85]  -- i64.xor
    ModuleF32 -> error "f32.xor not supported"
    ModuleF64 -> error "f64.xor not supported"
  
  ModuleBitNot vt -> case vt of
    ModuleI32 -> [0x74]  -- i32.not
    ModuleI64 -> [0x86]  -- i64.not
    ModuleF32 -> error "f32.not not supported"
    ModuleF64 -> error "f64.not not supported"

  ModuleShl vt -> case vt of
    ModuleI32 -> [0x75]  -- i32.shl
    ModuleI64 -> [0x87]  -- i64.shl
    ModuleF32 -> error "f32.shl not supported"
    ModuleF64 -> error "f64.shl not supported"
  
  ModuleShr vt -> case vt of
    ModuleI32 -> [0x76]  -- i32.shr_s
    ModuleI64 -> [0x88]  -- i64.shr_s
    ModuleF32 -> error "f32.shr not supported"
    ModuleF64 -> error "f64.shr not supported"

  ModuleLocalGet idx -> [0x20] <> encodeU32LEB128 idx
  ModuleLocalSet idx -> [0x21] <> encodeU32LEB128 idx
  ModuleGlobalGet idx -> [0x23] <> encodeU32LEB128 idx
  ModuleGlobalSet idx -> [0x24] <> encodeU32LEB128 idx

  ModuleCall idx -> [0x10] <> encodeU32LEB128 idx

  ModuleGt vt -> case vt of
    ModuleI32 -> [0x4A]  -- i32.gt_s
    ModuleI64 -> [0x55]  -- i64.gt_s
    ModuleF32 -> [0x5E]  -- f32.gt
    ModuleF64 -> [0x64]  -- f64.gt

  ModuleLt vt -> case vt of
    ModuleI32 -> [0x48]  -- i32.lt_s
    ModuleI64 -> [0x53]  -- i64.lt_s
    ModuleF32 -> [0x5D]  -- f32.lt
    ModuleF64 -> [0x63]  -- f64.lt

  ModuleEq vt -> case vt of
    ModuleI32 -> [0x46]  -- i32.eq
    ModuleI64 -> [0x51]  -- i64.eq
    ModuleF32 -> [0x5B]  -- f32.eq
    ModuleF64 -> [0x61]  -- f64.eq
  
  ModuleNeq vt -> case vt of
    ModuleI32 -> [0x47]  -- i32.ne
    ModuleI64 -> [0x52]  -- i64.ne
    ModuleF32 -> [0x5C]  -- f32.ne
    ModuleF64 -> [0x62]  -- f64.ne

  ModuleGe vt -> case vt of
    ModuleI32 -> [0x4E]  -- i32.ge_s
    ModuleI64 -> [0x59]  -- i64.ge_s
    ModuleF32 -> [0x60]  -- f32.ge
    ModuleF64 -> [0x66]  -- f64.ge

  ModuleLe vt -> case vt of
    ModuleI32 -> [0x4C]  -- i32.le_s
    ModuleI64 -> [0x57]  -- i64.le_s
    ModuleF32 -> [0x5F]  -- f32.le
    ModuleF64 -> [0x65]  -- f64.le

  ModuleEqz ModuleI32 -> [0x45]  -- i32.eqz
  ModuleEqz ModuleI64 -> [0x50]  -- i64.eqz
  ModuleEqz _ -> error "eqz not supported for float types"

  ModuleAnd vt -> case vt of
    ModuleI32 -> [0x71]  -- i32.and
    ModuleI64 -> [0x83]  -- i64.and
    ModuleF32 -> error "f32.and not supported"
    ModuleF64 -> error "f64.and not supported"
  
  ModuleOr vt -> case vt of
    ModuleI32 -> [0x72]  -- i32.or
    ModuleI64 -> [0x84]  -- i64.or
    ModuleF32 -> error "f32.or not supported"
    ModuleF64 -> error "f64.or not supported"

  ModuleIf maybeResType thenInstrs elseInstrs ->
    let blockTypeByte = case maybeResType of
          Nothing   -> 0x40               -- empty block type
          Just vt2  -> encodeValueType vt2
        thenBytes = concatMap encodeInstruction thenInstrs
        elseBytes = concatMap encodeInstruction elseInstrs
    in [0x04, blockTypeByte]   -- if
       ++ thenBytes
       ++ [0x05]               -- else
       ++ elseBytes
       ++ [0x0B]               -- end
    
  ModuleBlock maybeResType instrs ->
    let blockTypeByte = case maybeResType of
          Nothing   -> 0x40               -- empty block type
          Just vt2  -> encodeValueType vt2
        bodyBytes = concatMap encodeInstruction instrs
    in [0x02, blockTypeByte]   -- block
       ++ bodyBytes
       ++ [0x0B]               -- end
  
  ModuleLoop maybeResType instrs ->
    let blockTypeByte = case maybeResType of
          Nothing   -> 0x40               -- empty block type
          Just vt2  -> encodeValueType vt2
        bodyBytes = concatMap encodeInstruction instrs
    in [0x03, blockTypeByte]   -- loop
       ++ bodyBytes
       ++ [0x0B]               -- end
  
  ModuleBrIf idx -> [0x0D] <> encodeU32LEB128 idx
  ModuleBr idx -> [0x0C] <> encodeU32LEB128 idx

  ModuleReturn -> [0x0F]
  ModuleEnd -> [0x0B]



encodeTypeSection :: [TypeSection] -> [Word8]
encodeTypeSection ts =
  if null ts then []
  else
    let sectionId = 0x01
        encodeOneType (TypeSection (FunctionType ps rs)) =
          [0x60] <> encodeVector (map encodeValueType ps)
                 <> encodeVector (map encodeValueType rs)
        body = encodeVectorOfBlocks (map encodeOneType ts)
        sectionSize = encodeU32LEB128 (length body)
    in [sectionId] <> sectionSize <> body

encodeImportSection :: [ImportSection] -> [Word8]
encodeImportSection isecs =
  if null isecs then []
  else
    let sectionId = 0x02
        encodeImport (ImportSection (Import mname fname _vtype)) =
          encodeName mname
          <> encodeName fname
          <> [0x00]
          <> encodeU32LEB128 0
        encodeName s =
          let bs = C8.pack s
          in encodeU32LEB128 (BS.length bs) ++ BS.unpack bs

        body = encodeVectorOfBlocks (map encodeImport isecs)
        sectionSize = encodeU32LEB128 (length body)
    in [sectionId] <> sectionSize <> body

encodeFunctionSection :: [Function] -> [Word8]
encodeFunctionSection funs =
  if null funs then []
  else
    let sectionId = 0x03
        encodeOneFun f = encodeU32LEB128 (typeIndex f)
        body = encodeVectorOfBlocks (map encodeOneFun funs)
        sectionSize = encodeU32LEB128 (length body)
    in [sectionId] <> sectionSize <> body

encodeMemorySection :: [(Int, Int)] -> [Word8]
encodeMemorySection mems =
  if null mems then []
  else
    let sectionId = 0x05
        encodeMem (minPages, maxPages) =
          [0x01] <> encodeU32LEB128 minPages <> encodeU32LEB128 maxPages
        body = encodeVectorOfBlocks (map encodeMem mems)
        sectionSize = encodeU32LEB128 (length body)
    in [sectionId] <> sectionSize <> body

encodeGlobalSection :: [(ValueType, Mutability, [Instruction])] -> [Word8]
encodeGlobalSection globs =
  if null globs then []
  else
    let sectionId = 0x06

        encodeGlobal (valType, mut, initInstrs) =
          [ encodeValueType valType
          , case mut of
              ModuleMutable   -> 0x01
              ModuleImmutable -> 0x00
          ]
          <> concatMap encodeInstruction initInstrs
          <> [0x0B] -- end

        body = encodeVectorOfBlocks (map encodeGlobal globs)
        sectionSize = encodeU32LEB128 (length body)
    in [sectionId] <> sectionSize <> body

encodeExportSection :: [ExportSection] -> [Word8]
encodeExportSection exps =
  if null exps then []
  else
    let sectionId = 0x07
        encodeExport (ExportSection (Export nm kind)) index =
          let nmBytes  = C8.pack nm
              nmLen    = encodeU32LEB128 (BS.length nmBytes)
              kindByte = case kind of
                FunctionExport -> 0x00
                TableExport    -> 0x01
                MemoryExport   -> 0x02
                GlobalExport   -> 0x03
          in nmLen ++ BS.unpack nmBytes ++ [kindByte] ++ encodeU32LEB128 index

        body = encodeVectorOfBlocks (zipWith encodeExport exps [0..])
        sectionSize = encodeU32LEB128 (length body)
    in [sectionId] <> sectionSize <> body

encodeCodeSection :: [CodeSection] -> [Word8]
encodeCodeSection code =
  if null code then []
  else
    let sectionId = 0x0A
        encodeOneCode (CodeSection locs instrs) =
          let
            localsBytes =
              encodeVectorOfBlocks $
                map (\t -> encodeU32LEB128 1 <> [encodeValueType t]) locs

            bodyInstrs = concatMap encodeInstruction instrs
            fullBody   = localsBytes <> bodyInstrs <> [0x0B]  -- end
            size       = encodeU32LEB128 (length fullBody)
          in size ++ fullBody

        body = encodeVectorOfBlocks (map encodeOneCode code)
        sectionSize = encodeU32LEB128 (length body)
    in [sectionId] <> sectionSize <> body

encodeWasmModule :: WASMModule -> [Word8]
encodeWasmModule wasm =
  let header = [0x00, 0x61, 0x73, 0x6D,  -- "\0asm"
                0x01, 0x00, 0x00, 0x00] -- version = 1
      typeSec   = encodeTypeSection   (types wasm)
      importSec = encodeImportSection (imports wasm)
      funcSec   = encodeFunctionSection (functions wasm)
      memSec    = encodeMemorySection (memories wasm)
      globSec   = encodeGlobalSection (globals wasm)
      expSec    = encodeExportSection (exports wasm)
      codeSec   = encodeCodeSection (codes wasm)

      allSections =  typeSec
                  ++ importSec
                  ++ funcSec
                  ++ memSec
                  ++ globSec
                  ++ expSec
                  ++ codeSec
  in header ++ allSections

writeWasmModule :: FilePath -> WASMModule -> IO ()
writeWasmModule outFile wasm = do
  let bytes = encodeWasmModule wasm
      bs    = BS.pack bytes
  BS.writeFile outFile bs
  putStrLn $ "Wrote Wasm module to " ++ outFile
