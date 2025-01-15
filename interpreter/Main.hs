import System.Exit (ExitCode (ExitFailure), exitWith)
import System.Environment (getArgs)

import Interpreter.Data.Environment
import Interpreter.System.Evaluator
import Utils.Data.Result
import Parser.Data.Ast
import Interpreter.System.Module
import Interpreter.System.Command (runCommand)
import System.Console.Haskeline
import Control.Monad.IO.Class (liftIO)

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

printFailure :: String -> IO ()
printFailure s = do
    putStrLn s
    exitWith $ ExitFailure 84

terminateBy :: String -> Char -> String
terminateBy str end = if last str == end then str else str ++ [end]

maybePrint :: Show a => Maybe a -> IO ()
maybePrint Nothing = return ()
maybePrint (Just x) = print x

processLine :: Env -> String -> IO Env
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
        Ok e -> runInputT defaultSettings (loop e)           

main :: IO ()
main = do
    args <- getArgs
    putStrLn "Xenon Interpreter 1.0.0\nType '/help' for a list of commands."
    interpret args
