module Parser.Data.ParserS where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

numbers :: Parser [Int]
numbers = sepBy number comma <* eof
  where number = read <$> some digitChar
        comma  = recover $ char ','
        recover = withRecovery $ \e -> do
          registerParseError e
          some (anySingleBut ',')
          char ','

maintest :: IO ()
maintest = do
  let testInputs = [ "1,2,3,4,5"
                   , "1.2,3e5,4,5x"
                   , "1,2,e,4,5x"
                   , "1,2,3.4,5"
                   , "1,2,3.4"
                   , "1,2,3.4\n hundreds of lines without commas\nfinal line, with comma"
                   ]
  mapM_ runTest testInputs
  where
    runTest input = do
      putStrLn $ "Input: " ++ input
      case runParser numbers "FILE" input of
        Left err -> putStrLn $ errorBundlePretty err
        Right result -> putStrLn $ "Parsed successfully: " ++ show result
      putStrLn ""
