import System.Environment (getArgs)
import Interpreter.Data.Environment
import Interpreter.System.Evaluator
import Utils.Data.Result
import Parser.Data.Ast
import Interpreter.System.Module
import Interpreter.System.Command (runCommand)
import System.Console.Haskeline
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void)
import Utils.System.Print

isFnCall :: Body -> Bool
isFnCall [StandaloneFunctionCall _ _] = True
isFnCall _ = False

evalProgExprStr :: Env -> String -> Result String (Env, Maybe Expression)
evalProgExprStr e raw = case parseProg raw of
    Ok (Program b) | not (null b) && not (isFnCall b) -> case evalBody e b of
        Ok (e', _) -> Ok (e', Nothing)
        Err m -> Err m
    _ -> case parseExpr raw of
        Ok expr -> case evalExpr e expr of
            Ok (e', expr') -> Ok (e', Just expr')
            Err m -> Err m
        Err m -> Err m

terminateBy :: String -> Char -> String
terminateBy str end = if last str == end then str else str ++ [end]

processLine :: Env -> String -> IO Env
processLine e ['/'] = putStrLn "Invalid command" >> return e
processLine e ('/':xs) = do
    res <- runCommand e (words xs)
    case res of
        Ok e' -> return e'
        Err m -> putStrLn m >> return e
processLine e line
    | null line = return e
    | otherwise = case evalProgExprStr e (line `terminateBy` ';') of
        Ok (e', expr) -> maybePrint expr >> return e'
        Err m -> putStrLn m >> return e

loop :: Env -> InputT IO ()
loop e = do
    mline <- getInputLine ">> "
    case mline of
        Nothing -> loop e
        Just line -> do
            e' <- liftIO $ processLine e line
            loop e'

interpret :: [String] -> IO ()
interpret md = do
    res <- loadModules (env True) md
    case res of
        Err m -> printFailure m
        Ok e -> do
            putStrLn "Xenon Interpreter 1.0.0\nType '/help' for a list of commands."
            runInputT defaultSettings (loop e)

execute :: String -> IO ()
execute fp = do
    raw <- readFile fp
    case parseProg raw of
        Err m -> putStrLn m
        Ok (Program b) -> void $ evalBodyPrint (env True) b

getExecPath :: [String] -> Result String (Maybe String)
getExecPath [] = Ok Nothing
getExecPath ["-e"] = Err "No file was given"
getExecPath ("-e":f:_) = Ok $ Just f
getExecPath (_:xs) = getExecPath xs

isHelp :: [String] -> Bool
isHelp [] = False
isHelp ("-h":_) = True
isHelp (_:xs) = isHelp xs

helpMsg :: String
helpMsg = "Usage: xin [FILE] [FILE] ...\n\n\t-e FILE\t\tExecute the Xenon file."

main :: IO ()
main = do
    args <- getArgs
    if isHelp args then putStrLn helpMsg else
        case getExecPath args of
            Err m -> printFailure m
            Ok (Just fp) -> if length args == 2 then execute fp else
                printFailure "Invalid arguments"
            _ -> interpret args
