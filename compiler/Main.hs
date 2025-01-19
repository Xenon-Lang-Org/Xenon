import Parser.System.Parser
import Compiler.System.WriteWASM
import Compiler.System.FillModuleData
import Compiler.System.WriteWAT
import Utils.Data.Result
import System.Environment (getArgs)
import Analyzer.SemanticAnalyzer
import System.Exit (exitWith, ExitCode (ExitFailure))
import Parser.Data.Ast
import Interpreter.System.Optimizer

compilerParse :: String -> IO Program
compilerParse filename = do
    parseResult <- parseFileAndPrintErrors filename
    case parseResult of
        Ok p -> return p
        Err errs -> do
            print errs
            exitWith $ ExitFailure 84

compilerOptimize :: Program -> IO Program
compilerOptimize p = case optimizeProg p of
    Ok p' -> return p'
    Err m -> do
        putStrLn m
        exitWith $ ExitFailure 84

compilerAnalyse :: Program -> IO Program
compilerAnalyse p = case analyze p of
    Right p' -> return $ finalAst p'
    Left errs -> do
        print errs
        exitWith $ ExitFailure 84

compile :: String -> String -> IO ()
compile filename output = do
    prog <- compilerParse filename
    analyzedProg <- compilerAnalyse prog
    optimizedProg <- compilerOptimize analyzedProg
    let filledModule = fillWASMModuleFromAST optimizedProg
    printModule filledModule
    writeWasmModule output filledModule

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
        ["-h"] -> putStrLn "Usage: ./xcc <filename.xn> [-o output.wasm]"
        [filename, "-o", output] | checkArgs [filename, "-o", output] -> compile filename output
                                 | otherwise -> putStrLn "\ESC[31mError: filename must end with .xn and output must end with .wasm\ESC[97m"
        [filename] | checkArgs [filename] -> compile filename "result.wasm"
                   | otherwise -> putStrLn "\ESC[31mError: filename must end with .xn\ESC[97m"
        _ -> putStrLn "Usage: ./xcc <filename.xn> [-o output.wasm]"