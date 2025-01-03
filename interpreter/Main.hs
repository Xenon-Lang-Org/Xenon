import Parser.Data.Ast
import Interpreter.Evaluator


main :: IO ()
main = do
    print $ evalProg prog
    where
        prog = Program [ VariableDeclaration "foo" (PrimitiveType F32) (Just $ ELiteral $ FloatLiteral 50.5)
                       , VariableDeclaration "bar" (PrimitiveType U8) (Just $ ELiteral $ IntLiteral 256) 
                       , FunctionDeclaration "add" [("a", PrimitiveType I32), ("b", PrimitiveType I32)] (Just $ PrimitiveType I32) [ExpressionStatement $ BinaryOp Add (Variable "a") (Variable "b")]
                       , ExpressionStatement $ FunctionCall "add" [Variable "foo", Variable "bar"]]