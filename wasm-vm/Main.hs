import qualified Data.ByteString as BS
import System.Environment (getArgs)
import VM.Data.AST (Module, Value (..))
import VM.Data.VMTypes (VM (..))
import VM.System.Parser (parseWasm)
import VM.System.VM (invokeByNameWithArgs)

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
      putStrLn "Usage: wasm-vm <file.wasm> [--invoke function_name] [arg1 arg2 ...]"

mainParser :: BS.ByteString -> String -> [String] -> IO ()
mainParser bytes funcName rawArgs = do
  case parseWasm bytes of
    Left err -> putStrLn $ "Parse error: " ++ err
    Right modAST -> do
      let paramVals = map parseArg rawArgs
      case sequence paramVals of
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

parseArg :: String -> Either String Value
parseArg s =
  if '.' `elem` s
    then Right (F32 (read s :: Float))
    else Right (I32 (read s))
