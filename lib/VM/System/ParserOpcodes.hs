module VM.System.ParserOpcodes
  ( parseInstruction
  ) where

import Data.Word ( Word8)
import Data.Binary.Get (getWord8, Get )

import VM.Data.AST ( Instruction(..) )
import VM.System.ParserUtils
    ( getU32LEB, getS32LEB, getS64LEB, getF32LE, getF64LE ) 

parseInstruction :: Word8 -> Get Instruction
parseInstruction op = case op of
  -- CONST
  0x41 -> I32Const <$> getS32LEB
  0x42 -> I64Const <$> getS64LEB
  0x43 -> F32Const <$> getF32LE
  0x44 -> F64Const <$> getF64LE

  -- MATH
  0x6A -> pure I32Add
  0x7C -> pure I64Add
  0x92 -> pure F32Add
  0xA0 -> pure F64Add

  0x6B -> pure I32Sub
  0x7D -> pure I64Sub
  0x93 -> pure F32Sub
  0xA1 -> pure F64Sub

  0x6C -> pure I32Mul
  0x7E -> pure I64Mul
  0x94 -> pure F32Mul
  0xA2 -> pure F64Mul

  0x6D -> pure I32DivS
  0x7F -> pure I64DivS
  0x95 -> pure F32Div
  0xA3 -> pure F64Div

  0x6F -> pure I32RemS
  0x81 -> pure I64RemS

  0x71 -> pure I32And
  0x83 -> pure I64And

  0x72 -> pure I32Or
  0x84 -> pure I64Or

  0x73 -> pure I32Xor
  0x85 -> pure I64Xor

  0x74 -> pure I32Shl
  0x86 -> pure I64Shl

  0x75 -> pure I32Shr
  0x87 -> pure I64Shr

  0x4A -> pure I32GtS
  0x55 -> pure I64GtS
  0x5E -> pure F32Gt
  0x64 -> pure F64Gt

  0x48 -> pure I32LtS
  0X53 -> pure I64LtS
  0X5D -> pure F32Lt
  0X63 -> pure F64Lt

  0x46 -> pure I32Eq
  0x51 -> pure I64Eq
  0x5B -> pure F32Eq
  0x61 -> pure F64Eq

  0X47 -> pure I32Ne
  0x52 -> pure I64Ne
  0x5C -> pure F32Ne
  0x62 -> pure F64Ne

  0x4E -> pure I32GeS
  0x59 -> pure I64GeS
  0x60 -> pure F32Ge
  0x66 -> pure F64Ge

  0x4C -> pure I32LeS
  0x57 -> pure I64LeS
  0x5F -> pure F32Le
  0x65 -> pure F64Le

  0x45 -> pure I32Eqz
  0x50 -> pure I64Eqz

  -- MEMORY
  0x20 -> LocalGet . fromIntegral <$> getU32LEB
  0x21 -> LocalSet . fromIntegral <$> getU32LEB
  0x23 -> GlobalGet . fromIntegral <$> getU32LEB
  0x24 -> GlobalSet . fromIntegral <$> getU32LEB

  -- CONTROL
  0x10 -> Call . fromIntegral <$> getU32LEB
  
  0x04 -> do
    _ <- getWord8 -- discards the block type
    pure If
  0x05 -> pure Else

  0x02 -> do
    _ <- getWord8
    pure Block

  0x03 -> do
    _ <- getWord8
    pure Loop

  0x0D -> BrIf . fromIntegral <$> getU32LEB
  0x0C -> Br . fromIntegral <$> getU32LEB

  0x0F -> pure Return
  0x0B -> pure End

  -- ERROR
  _ -> fail $ "Unknown opcode: 0x" ++ showHex op ""

showHex :: (Integral a) => a -> String -> String
showHex n s = show (toInteger n) ++ s
