import Text.Megaparsec
import Parser.System.Parser
import qualified Parser.System.Lexer as Lexer
import Interpreter.System.Evaluator (evalProg)

main :: IO ()
main = do
    let runLexer = runParser Lexer.tokens "LEXER"
    raw <- readFile "main.xn"
    case runLexer raw of
        Left err -> putStrLn $ errorBundlePretty err
        Right tokens -> case runParser parseProgram "main.xn" (Lexer.TokenStream tokens raw) of
          Left err -> do
            putStrLn $ "tokens: " ++ show tokens
            putStrLn $ errorBundlePretty err
          Right result -> print $ evalProg result
        