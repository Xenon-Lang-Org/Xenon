import Parser.Data.Ast
import Interpreter.Data.Environment
import Interpreter.System.BinaryOperation
import Utils.Data.Result
import Interpreter.System.Evaluator

main :: IO ()
main = do
    print $ evalProg prog
    where
        prog = Program [ VariableDeclaration "nb" (PrimitiveType U32) (Just $ ELiteral $ IntLiteral 1)
                       , VariableDeclaration "itt" (PrimitiveType U32) (Just $ ELiteral $ IntLiteral 0) 
                       , FunctionDeclaration "shift" [("to", PrimitiveType I32)] (PrimitiveType I32) [
                            WhileLoop (BinaryOp Lt (Variable "itt") (Variable "to")) [ ExpressionStatement $ BinaryOp Assign (Variable "nb") (BinaryOp Mul (Variable "nb") (ELiteral $ IntLiteral 2))
                                                                                     , ExpressionStatement $ BinaryOp Assign (Variable "itt") (BinaryOp Add (Variable "itt") (ELiteral $ IntLiteral 1)) ]
                       ]
                       , ExpressionStatement $ FunctionCall "shift" [ELiteral $ IntLiteral 8]]