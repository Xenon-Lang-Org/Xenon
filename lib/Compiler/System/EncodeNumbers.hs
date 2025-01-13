module Compiler.System.EncodeNumbers (
    encodeU32LEB128,
    encodeS32LEB128,
    encodeF32,
    encodeF64
) where

import Data.Binary.Put (runPut, putWord32le, putWord64le)
import Data.ByteString.Lazy (toStrict)
import GHC.Float (castDoubleToWord64, castFloatToWord32)
import Data.Bits
import Data.Word
import qualified Data.ByteString as BS

-- | Encodes a 32-bit unsigned integer into LEB128 format, which is a variable-length
-- | encoding used in WebAssembly for integers. https://en.wikipedia.org/wiki/LEB128
encodeU32LEB128 :: Int -> [Word8]
encodeU32LEB128 n
  | n < 0     = error "encodeU32LEB128: negative number"
  | n == 0    = [0]
  | otherwise =
      let byte = fromIntegral (n .&. 0x7F)
          rest = n `shiftR` 7
      in if rest == 0
            then [byte]
            else (byte .|. 0x80) : encodeU32LEB128 rest


-- | Encodes a 32-bit signed integer into LEB128 format, which is a variable-length
-- | encoding used in WebAssembly for integers. https://en.wikipedia.org/wiki/LEB128
encodeS32LEB128 :: Int -> [Word8]
encodeS32LEB128 n =
  let byte = fromIntegral (n .&. 0x7F)
      rest = n `shiftR` 7
      signBitSet = (byte .&. 0x40) /= 0
      isLastByte = (rest == 0 && not signBitSet) || (rest == -1 && signBitSet)
  in if isLastByte
        then [byte]
        else (byte .|. 0x80) : encodeS32LEB128 rest

encodeF32 :: Float -> [Word8]
encodeF32 f =
  let bits  = castFloatToWord32 f
      bytes = runPut (putWord32le bits)
  in BS.unpack (toStrict bytes)

encodeF64 :: Double -> [Word8]
encodeF64 d =
  let bits  = castDoubleToWord64 d
      bytes = runPut (putWord64le bits)
  in BS.unpack (toStrict bytes)
