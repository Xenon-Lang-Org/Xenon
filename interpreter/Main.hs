import Interpreter.Scope
import Parser.Data.Ast

main :: IO ()
main = do
    print $ fromScope scope "bar"
    where
        scope = [ VariableDeclaration "foo" (PrimitiveType I8) (Just $ ELiteral $ IntLiteral 42)
                , VariableDeclaration "bar" (PrimitiveType I8) (Just $ ELiteral $ IntLiteral 42) ]