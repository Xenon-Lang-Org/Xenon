import qualified Parser.System.Lexer as Lexer
import Text.Megaparsec
import Parser.System.Parser
import Interpreter.Data.Environment
import Interpreter.System.Evaluator
import System.Environment (getArgs)
import Utils.Data.Result
import Parser.Data.Ast
import System.Exit (exitSuccess, ExitCode (ExitFailure), exitWith)
import System.IO

parseProg :: String -> Result String Program
parseProg raw = do
  let runLexer = runParser Lexer.tokens "lexer"
  case runLexer raw of
    Left err -> Err $ errorBundlePretty err
    Right tokens -> case runParser parseProgram "input" (Lexer.TokenStream tokens raw) of
      Left err -> Err $ "tokens: " ++ show tokens ++ "\n" ++ errorBundlePretty err
      Right result -> Ok result

parseExpr :: String -> Result String Expression
parseExpr raw = do
  let runLexer = runParser Lexer.tokens "lexer"
  case runLexer raw of
    Left err -> Err $ errorBundlePretty err
    Right tokens -> case runParser parseExpression "input" (Lexer.TokenStream tokens raw) of
      Left err -> Err $ "tokens: " ++ show tokens ++ "\n" ++ errorBundlePretty err
      Right result -> Ok result

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

interpretFile :: String -> IO ()
interpretFile path = do
  raw <- readFile path
  case mapOk evalProg (parseProg raw) of
    Ok (Ok (_, ex)) -> print ex
    Ok (Err m) -> printFailure m
    Err m -> printFailure m

terminateBy :: String -> Char -> String
terminateBy str end = if last str == end then str else str ++ [end]

maybePrint :: Show a => Maybe a -> IO ()
maybePrint Nothing = return ()
maybePrint (Just x) = print x

interpretStdin :: Env -> IO ()
interpretStdin e = do
  putStr "> "
  hFlush stdout
  line <- getLine
  case line of
    "" -> interpretStdin e
    "exit" -> exitSuccess
    _ -> case evalProgExprStr e (line `terminateBy` ';') of
      Ok (e', expr) -> do
        maybePrint expr
        interpretStdin e'
      Err m -> do
        putStrLn m
        interpretStdin e

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> interpretStdin (env True)
    [f] -> interpretFile f
    _ -> putStrLn "USAGE: xin [file]\n\tfile (optional)\t\tfile to interpret"
