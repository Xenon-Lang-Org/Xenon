import Parser.System.Parser
import Compiler.System.WriteWASM
import Compiler.System.FillModuleData
import Compiler.System.WriteWAT
import Utils.Data.Result
import System.Environment (getArgs)
import Analyzer.SemanticAnalyzer

compile :: String -> String -> IO ()
compile filename output = do
    parseResult <- parseFileAndPrintErrors filename
    case parseResult of
        Ok ast -> do
            putStrLn (show ast)
            case analyze ast of
                Right result -> do
                    let filledModule = fillWASMModuleFromAST (finalAst result)
                    printModule filledModule
                    writeWasmModule output filledModule
                Left analyzeErr -> error $ show analyzeErr
        Err err -> print err

checkFileName :: String -> Bool
checkFileName filename = (length filename) > 3 && (drop (length filename - 3) filename) == ".xn"

checkOutputName :: String -> Bool
checkOutputName output = (length output) > 5 && (drop (length output - 5) output) == ".wasm"

checkArgs :: [String] -> Bool
checkArgs [filename, "-o", output] = checkFileName filename && checkOutputName output
checkArgs [filename] = checkFileName filename
checkArgs _ = False

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-h"] -> putStrLn "Usage: ./compiler <filename.xn> [-o output.wasm]"
        [filename, "-o", output] | checkArgs [filename, "-o", output] -> compile filename output
                                 | otherwise -> putStrLn "\ESC[31mError: filename must end with .xn and output must end with .wasm\ESC[97m"
        [filename] | checkArgs [filename] -> compile filename "result.wasm"
                   | otherwise -> putStrLn "\ESC[31mError: filename must end with .xn\ESC[97m"
        _ -> putStrLn "Usage: ./compiler <filename.xn> [-o output.wasm]"