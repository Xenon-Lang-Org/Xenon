module VM.System.ParserUtils
  ( getU32LEB,
    getS32LEB,
    getSLEB128,
    getS64LEB,
    getF32LE,
    getF64LE,
  )
where

import Data.Binary.Get (Get, getDoublele, getFloatle, getWord8)
import Data.Bits (Bits (shiftL, testBit, (.&.), (.|.)))
import Data.Int (Int32, Int64)
import Data.Word (Word32)

getU32LEB :: Get Word32
getU32LEB = do
  (val, _) <- getULEB128 0 0
  return val

getS32LEB :: Get Int32
getS32LEB = do
  (val, _) <- getSLEB128 0 0
  return val

getULEB128 :: Word32 -> Int -> Get (Word32, Bool)
getULEB128 acc shi = do
  b <- getWord8
  let val = acc .|. (fromIntegral (b .&. 0x7F) `shiftL` shi)
      cont = testBit b 7
      shi' = shi + 7
  if cont then getULEB128 val shi' else return (val, False)

getSLEB128 :: Int32 -> Int -> Get (Int32, Bool)
getSLEB128 acc shi = do
  b <- getWord8
  let val = acc .|. (fromIntegral (b .&. 0x7F) `shiftL` shi)
      cont = testBit b 7
      shi' = shi + 7
  if cont
    then getSLEB128 val shi'
    else do
      let size = 32
      if shi' < size && testBit b 6
        then return (val .|. ((-1) `shiftL` shi'), False)
        else return (val, False)

getS64LEB :: Get Int64
getS64LEB = do
  (val, _) <- getSLEB128_64 0 0
  return val

getSLEB128_64 :: Int64 -> Int -> Get (Int64, Bool)
getSLEB128_64 acc shi = do
  b <- getWord8
  let val = acc .|. (fromIntegral (b .&. 0x7F) `shiftL` shi)
      cont = testBit b 7
      shi' = shi + 7
  if cont
    then getSLEB128_64 val shi'
    else do
      let size = 64
      if shi' < size && testBit b 6
        then return (val .|. ((-1) `shiftL` shi'), False)
        else return (val, False)

getF32LE :: Get Float
getF32LE = getFloatle

getF64LE :: Get Double
getF64LE = getDoublele
