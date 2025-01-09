import Parser.Data.Ast
import Interpreter.Data.Environment
import Interpreter.System.Operator
import Utils.Data.Result

main :: IO ()
main = do
    print $ evalBinOp env Shl (ELiteral $ IntLiteral 1) (ELiteral $ IntLiteral 2)
    where
        env = case pushVariable (newEnv False) (VariableDeclaration "my_var" (PrimitiveType I8) (Just $ ELiteral $ IntLiteral 0)) of
            Ok env' -> env'
            _ -> newEnv False
        op = BinaryOp Assign (Variable "my_var") (ELiteral $ IntLiteral 12)
        prog = Program [ VariableDeclaration "foo" (PrimitiveType F32) (Just $ ELiteral $ FloatLiteral 50.5)
                       , VariableDeclaration "bar" (PrimitiveType U8) (Just $ ELiteral $ IntLiteral 256) 
                       , FunctionDeclaration "add" [("a", PrimitiveType I32), ("b", PrimitiveType I32)] (PrimitiveType I32) [ExpressionStatement $ BinaryOp Add (Variable "a") (Variable "b")]
                       , ExpressionStatement $ FunctionCall "add" [Variable "foo", Variable "bar"]]