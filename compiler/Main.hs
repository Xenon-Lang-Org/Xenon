import Parser.Data.Ast
import ModuleData
import WriteWASM
import FillModuleData
import WriteWAT

-- input to test function call compilation
testFunctionCallAST :: Program
testFunctionCallAST = Program [
    VariableDeclaration "bar" (PrimitiveType I32) (Just $ ELiteral $ IntLiteral (-256)),
    FunctionDeclaration "addneg" [("a", PrimitiveType I32)] (PrimitiveType I32)
        [ExpressionStatement $ BinaryOp Add (Variable "a") (Variable "bar")],
    FunctionDeclaration "dowhile" [("a",PrimitiveType I32)] (PrimitiveType I32) [VariableDeclaration "x" (PrimitiveType I32) (Just (ELiteral (IntLiteral 0))),WhileLoop (BinaryOp Lt (Variable "x") (Variable "a")) [ExpressionStatement (BinaryOp Assign (Variable "x") (BinaryOp Add (Variable "x") (ELiteral (IntLiteral 1))))],ReturnStatement (Variable "x")],
    FunctionDeclaration "main" [("a", PrimitiveType I32)] (PrimitiveType I32)
        [ReturnStatement $ FunctionCall "dowhile" [Variable "a"]]
    ]

-- input to test 4 functions compilation
testMultipleFunctionAST :: Program
testMultipleFunctionAST = Program 
  [ FunctionDeclaration 
      "add" 
      [("a", PrimitiveType I32), ("b", PrimitiveType I32)] 
      (PrimitiveType I32) 
      [ ReturnStatement $ BinaryOp Add (Variable "a") (Variable "b") ]
  , FunctionDeclaration 
      "sub" 
      [("a", PrimitiveType I32), ("b", PrimitiveType I32)] 
      (PrimitiveType I32) 
      [ ReturnStatement $ BinaryOp Sub (Variable "a") (Variable "b") ]
  , FunctionDeclaration
      "mul"
      [("a", PrimitiveType I32), ("b", PrimitiveType I32)]
      (PrimitiveType I32)
      [ ReturnStatement $ BinaryOp Mul (Variable "a") (Variable "b") ]
  , FunctionDeclaration
      "div"
      [("a", PrimitiveType I32), ("b", PrimitiveType I32)]
      (PrimitiveType I32)
      [ ReturnStatement $ BinaryOp Div (Variable "a") (Variable "b") ]
  ]

-- input to test while loop compilation
testWhileAST :: Program
testWhileAST = Program [FunctionDeclaration "dowhile" [("a",PrimitiveType I32)] (PrimitiveType I32) [VariableDeclaration "x" (PrimitiveType I32) (Just (ELiteral (IntLiteral 0))),WhileLoop (BinaryOp Lt (Variable "x") (Variable "a")) [ExpressionStatement (BinaryOp Assign (Variable "x") (BinaryOp Add (Variable "x") (ELiteral (IntLiteral 1))))],ReturnStatement (Variable "x")]]

-- input to test negative variable compilation
testNegNbAST :: Program
testNegNbAST = Program [
    VariableDeclaration "bar" (PrimitiveType I32) (Just $ ELiteral $ IntLiteral (-256)),
    FunctionDeclaration "addneg" [("a", PrimitiveType I32)] (PrimitiveType I32)
        [ExpressionStatement $ BinaryOp Add (Variable "a") (Variable "bar")]
    ]

-- input to test global variable compilation
testGlobalAST :: Program
testGlobalAST = Program [
    VariableDeclaration "bar" (PrimitiveType I32) (Just $ ELiteral $ IntLiteral 256),
    FunctionDeclaration "addglobal" [("a", PrimitiveType I32)] (PrimitiveType I32)
        [ExpressionStatement $ BinaryOp Add (Variable "a") (Variable "bar")]
    ]

-- input to test basic add function
testBasicFunctionAST :: Program
testBasicFunctionAST = Program [
    FunctionDeclaration "add" [("a", PrimitiveType I32), ("b", PrimitiveType I32)] (PrimitiveType I32)
        [ExpressionStatement $ BinaryOp Add (Variable "a") (Variable "b")]
    ]

main :: IO ()
main = do
    printModule filledModule
    writeWasmModule "result.wasm" filledModule
  where
    filledModule = fillWASMModuleFromAST testFunctionCallAST
