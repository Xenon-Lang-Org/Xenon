import qualified Data.ByteString as BS
import System.Environment (getArgs)
import Text.Read (readEither)
import VM.Data.AST (Module, ValType (..), Value (..))
import VM.Data.VMTypes (VM (..))
import VM.System.Parser (parseWasm)
import VM.System.VM (getParamTypesByName, invokeByNameWithArgs)
import Data.Int ( Int32, Int64 )

main :: IO ()
main = do
  args <- getArgs
  case args of
    (fileName : "--invoke" : funcName : rawArgs) -> do
      bytes <- BS.readFile fileName
      mainParser bytes funcName rawArgs
    (fileName : rawArgs) -> do
      bytes <- BS.readFile fileName
      mainParser bytes "main" rawArgs
    _ ->
      putStrLn "Usage: xrun <file.wasm> [--invoke function_name] [arg1 arg2 ...]"

mainParser :: BS.ByteString -> String -> [String] -> IO ()
mainParser bytes funcName rawArgs = do
  case parseWasm bytes of
    Left err -> putStrLn $ "Parse error: " ++ err
    Right modAST -> do
      case getParamTypesByName modAST funcName of
        Left err -> putStrLn $ "Runtime error: " ++ err
        Right paramTypes -> do
          case parseArgs paramTypes rawArgs of
            Left err -> putStrLn $ "Argument error: " ++ err
            Right vals -> launchVM modAST funcName vals

launchVM :: Module -> String -> [Value] -> IO ()
launchVM modAST funcName vals = do
  case invokeByNameWithArgs modAST funcName vals of
    Left vmErr -> putStrLn $ "Runtime error: " ++ vmErr
    Right vmEnd -> putStrLn $ unwords (map showValue (operandStack vmEnd))

showValue :: Value -> String
showValue (I32 i) = show i
showValue (I64 i) = show i
showValue (F32 f) = show f
showValue (F64 f) = show f

parseArg :: ValType -> String -> Either String Value
parseArg V_I32 str = do
  n <- readEither str :: Either String Integer
  if n >= fromIntegral (minBound :: Int32) && n <= fromIntegral (maxBound :: Int32)
    then Right $ I32 (fromIntegral n)
    else Left $ "Value out of range for i32: " ++ str
parseArg V_I64 str = do
  n <- readEither str :: Either String Integer
  if n >= fromIntegral (minBound :: Int64) && n <= fromIntegral (maxBound :: Int64)
    then Right $ I64 (fromIntegral n)
    else Left $ "Value out of range for i64: " ++ str
parseArg V_F32 str = do
  n <- readEither str :: Either String Double
  let f = realToFrac n :: Float
  if abs (realToFrac f - n) < 1e-6
    then Right $ F32 f
    else Left $ "Value out of range or precision loss for f32: " ++ str
parseArg V_F64 str = do
  n <- readEither str :: Either String Double
  Right $ F64 n

parseArgs :: [ValType] -> [String] -> Either String [Value]
parseArgs [] [] = Right []
parseArgs (vt : vts) (str : strs) = do
  value <- parseArg vt str
  rest <- parseArgs vts strs
  return (value : rest)
parseArgs [] (_ : _) = Left "Too many arguments provided"
parseArgs (_ : _) [] = Left "Not enough arguments provided"