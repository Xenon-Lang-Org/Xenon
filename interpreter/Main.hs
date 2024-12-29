import Parser.Data.Ast
import Interpreter.Environment
import Interpreter.Evaluator


main :: IO ()
main = do
    print $ callEnv env call declr
    where
        g = [ VariableDeclaration "foo" (PrimitiveType I8) (Just $ ELiteral $ IntLiteral 42)
            , VariableDeclaration "bar" (PrimitiveType I8) (Just $ ELiteral $ IntLiteral 42) ]
        l = [ VariableDeclaration "hello" (PrimitiveType I8) (Just $ ELiteral $ IntLiteral 42)
            , VariableDeclaration "world" (PrimitiveType I8) (Just $ ELiteral $ IntLiteral 42) ]
        env = Env g l
        declr = FunctionDeclaration "add" [("nb1", PrimitiveType I32), ("nb2", PrimitiveType I32)] (Just $ PrimitiveType I32) []
        call = FunctionCall "add" [ELiteral $ IntLiteral 12, ELiteral $ IntLiteral 42]