module Interpreter.System.Module
    (
        loadModules,
        loadModule,
        parseProg,
        parseExpr
    )
where

import Parser.System.Parser
import Text.Megaparsec (runParser, errorBundlePretty)
import qualified Parser.System.Lexer as Lexer
import Parser.Data.Ast
import Interpreter.Data.Environment
import Interpreter.System.Evaluator
import Utils.Data.Result
import Control.Exception (IOException, try)

parseProg :: String -> Result String Program
parseProg raw = do
  let runLexer = runParser Lexer.tokens "lexer"
  case runLexer raw of
    Left err -> Err $ errorBundlePretty err
    Right t -> case runParser parseProgram "input" (Lexer.TokenStream t raw) of
      Left err -> Err $ "token: " ++ show t ++ "\n" ++ errorBundlePretty err
      Right result -> Ok result

parseExpr :: String -> Result String Expression
parseExpr raw = do
  let runLexer = runParser Lexer.tokens "lexer"
  case runLexer raw of
    Left err -> Err $ errorBundlePretty err
    Right t -> case runParser parseExpression "input" (Lexer.TokenStream t raw) of
      Left err -> Err $ "token: " ++ show t ++ "\n" ++ errorBundlePretty err
      Right result -> Ok result

loadModule :: Env -> String -> IO (Result String Env)
loadModule e fp = do
  result <- try (readFile fp) :: IO (Either IOException String)
  case result of
    Left ex -> return $ Err $ show ex
    Right raw -> do
      let prog = parseProg raw
      case prog of
        Err m -> return $ Err m
        Ok (Program b) -> case evalBody e b of
          Err m -> return $ Err m
          Ok (e', _) -> return $ Ok e'

loadModules :: Env -> [String] -> IO (Result String Env)
loadModules e md = do
    foldl (\acc fp -> do
        res <- acc
        case res of
            Err m -> return $ Err m
            Ok e' -> loadModule e' fp) (return $ Ok e) md
