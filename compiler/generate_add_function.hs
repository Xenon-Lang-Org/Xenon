import qualified Data.ByteString as BS -- Using qualified import to avoid name conflicts
import qualified Data.ByteString.Char8 as BS8
import Data.Word (Word8, Word32)
import Data.Bits ((.&.), (.|.), shiftR)
import System.Environment (getArgs)

-- | Encodes a 32-bit unsigned integer into LEB128 format, which is a variable-length
-- | encoding used in WebAssembly for integers. https://en.wikipedia.org/wiki/LEB128
encodeLEB128 :: Word32 -> BS.ByteString
encodeLEB128 n
    | n < 0x80 = BS.singleton (fromIntegral n)
    | otherwise = BS.concat [BS.singleton (fromIntegral (n .&. 0x7F) .|. 0x80), 
                          encodeLEB128 (n `shiftR` 7)]


-- | Creates the standard WebAssembly module header with magic number (\0asm) 
-- | and version number (0x1)
wasmHeader :: BS.ByteString
wasmHeader = BS.concat [
    BS8.pack "\0asm", -- Magic number
    BS.pack [0x01, 0x00, 0x00, 0x00]  -- WASM Version
    ]


-- | Generates the type section of the WASM module
-- | Defines a function type that takes two i32 parameters and returns one i32
typeSection :: BS.ByteString
typeSection = BS.concat [
    BS.singleton 0x01,     -- Section code
    encodeLEB128 7,        -- Section size
    encodeLEB128 1,        -- Number of types
    BS.singleton 0x60,     -- Function type
    encodeLEB128 2,        -- Number of parameters
    BS.singleton 0x7F,     -- i32
    BS.singleton 0x7F,     -- i32
    encodeLEB128 1,        -- Number of results
    BS.singleton 0x7F      -- i32
    ]


-- | Generates the function section of the WASM module
-- | Declares one function that uses the type defined in the type section
functionSection :: BS.ByteString
functionSection = BS.concat [
    BS.singleton 0x03,    -- Section code
    encodeLEB128 2,       -- Section size
    encodeLEB128 1,       -- Number of functions
    encodeLEB128 0        -- Type index
    ]


-- | Generates the export section of the WASM module
-- | Exports a function named "add" that can be called from the host environment
exportSection :: BS.ByteString
exportSection = BS.concat [
    BS.singleton 0x07,    -- Section code
    encodeLEB128 7,       -- Section size
    encodeLEB128 1,       -- Number of exports
    encodeLEB128 3,       -- Name length
    BS8.pack "add",       -- Function name
    BS.singleton 0x00,    -- Export kind
    encodeLEB128 0        -- Function index
    ]


-- | Generates the code section of the WASM module
-- | Contains the actual implementation of the add function using WASM bytecode
codeSection :: BS.ByteString
codeSection = BS.concat [
    BS.singleton 0x0A,    -- Section code
    encodeLEB128 9,       -- Section size
    encodeLEB128 1,       -- Number of functions
    encodeLEB128 7,       -- Function body size
    encodeLEB128 0,       -- Local declarations count
    BS.singleton 0x20,    -- local.get 0
    encodeLEB128 0,       -- Index of first parameter
    BS.singleton 0x20,    -- local.get 1
    encodeLEB128 1,       -- Index of second parameter
    BS.singleton 0x6A,    -- i32.add
    BS.singleton 0x0B     -- end
    ]


-- | Combines all sections into a complete WebAssembly module
wasmModule :: BS.ByteString
wasmModule = BS.concat [wasmHeader, typeSection, functionSection, exportSection, codeSection]


-- | Debug utility function that prints the size and raw bytes of a WASM section
-- | Parameters:
-- |   name: Section name for display
-- |   builder: The section's binary content
debugSection :: String -> BS.ByteString -> IO ()
debugSection name bytes = do
    putStrLn $ name ++ " size: " ++ show (BS.length bytes)
    print $ BS.unpack bytes


writeWasm :: FilePath -> String -> IO ()
writeWasm filePath "debug" = do
    debugSection "Type" typeSection
    debugSection "Function" functionSection
    debugSection "Export" exportSection
    debugSection "Code" codeSection
    BS.writeFile filePath wasmModule
    putStrLn $ "WASM binary written to " ++ filePath
    print $ BS.unpack wasmModule
writeWasm filePath _ = do
    BS.writeFile filePath wasmModule
    putStrLn $ "WASM binary written to " ++ filePath



main :: IO ()
main = do
    args <- getArgs
    case args of
        [debug] -> writeWasm "add.wasm" "debug"
        _ -> writeWasm "add.wasm" "release"
