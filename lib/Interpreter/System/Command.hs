module Interpreter.System.Command
    (
        runCommand
    )
where

import Interpreter.Data.Environment
import Utils.Data.Result
import Interpreter.System.Module(loadModules)
import System.Exit (exitSuccess)

data Command = Command String (Env -> [String] -> IO (Result String Env)) String

commandName :: Command -> String
commandName (Command n _ _) = n

getCommand :: String -> [Command] -> Result String Command
getCommand _ [] = Err "Command not found"
getCommand n (x:xs) = if n == commandName x then Ok x else getCommand n xs

runCommand :: Env -> [String] -> IO (Result String Env)
runCommand e a = case getCommand (head a) commands of
    Ok (Command _ f _) -> f e (tail a)
    Err m -> return $ Err m

-- Command functions

loadModuleCmd :: Env -> [String] -> IO (Result String Env)
loadModuleCmd = loadModules

envLookup :: Env -> [String] -> IO (Result String Env)
envLookup e [] = return $ Ok e
envLookup e (x:xs) = case fromEnv e x of
    Err m -> return $ Err m
    Ok v -> print v >> envLookup e xs

lookupEnvCmd :: Env -> [String] -> IO (Result String Env)
lookupEnvCmd e a
    | null a = envLookup e (envAllNames e)
    | otherwise = envLookup e a

exitCmd :: Env -> [String] -> IO (Result String Env)
exitCmd _ _ = exitSuccess

helpCmd :: Env -> [String] -> IO (Result String Env)
helpCmd e [] = mapM_ (\(Command name _ desc) -> putStrLn $ name ++ "\t- " ++ desc) commands
    >> return (Ok e)
helpCmd e a = help a
    where 
        printHelp (Command name _ desc) = putStrLn (name ++ "\t- " ++ desc)
        help [] = return $ Ok e
        help (x:xs) = case getCommand x commands of
            Err m -> return $ Err m
            Ok cmd -> printHelp cmd >> help xs

-- Commands

commands :: [Command]
commands = [ Command "help" helpCmd "help <command1> <command2> ..." 
           , Command "load" loadModuleCmd "load <file1> <file2> ..."
           , Command "env" lookupEnvCmd "env <var1> <var2> ..."
           , Command "exit" exitCmd "exit"
           ]
