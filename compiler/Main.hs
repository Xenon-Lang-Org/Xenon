import Parser.Data.Ast
import WriteWASM
import FillModuleData
import WriteWAT

-- input to test Double
testF64AST :: Program
testF64AST = Program [
    FunctionDeclaration "addDouble" [("a", PrimitiveType Immutable F64), ("b", PrimitiveType Immutable F64)] (PrimitiveType Immutable F64)
        [ReturnStatement $ BinaryOp Add (Variable "a") (Variable "b")]
    ]

-- input to test I64
testI64AST :: Program
testI64AST = Program [
    FunctionDeclaration "add64" [("a", PrimitiveType Immutable I64), ("b", PrimitiveType Immutable I64)] (PrimitiveType Immutable I64)
        [ReturnStatement $ BinaryOp Add (Variable "a") (Variable "b")]
    ]

-- input to test float
testFloatAST :: Program
testFloatAST = Program [
    FunctionDeclaration "addfloat" [("a", PrimitiveType Immutable F32), ("b", PrimitiveType Immutable F32)] (PrimitiveType Immutable F32)
        [ReturnStatement $ BinaryOp Add (Variable "a") (Variable "b")]
    ]

-- input to test if compilation
testIfAST :: Program
testIfAST = Program [FunctionDeclaration "doif" [("a",PrimitiveType Immutable I32)] (PrimitiveType Immutable I32) [VariableDeclaration "x" (PrimitiveType Mutable I32) (Just (ELiteral (IntLiteral 0))),If (BinaryOp Eq (Variable "a") (ELiteral (IntLiteral 0))) [VariableReAssignment "x" (ELiteral (IntLiteral 1))] (Just [VariableReAssignment "x" (ELiteral (IntLiteral 0))]),ReturnStatement (Variable "x")]]

-- input to test function call compilation
testFunctionCallAST :: Program
testFunctionCallAST = Program [
    VariableDeclaration "bar" (PrimitiveType Immutable I32) (Just $ ELiteral $ IntLiteral (-256)),
    FunctionDeclaration "addneg" [("a", PrimitiveType Immutable I32)] (PrimitiveType Immutable I32)
        [ReturnStatement $ BinaryOp Add (Variable "a") (Variable "bar")],
    FunctionDeclaration "dowhile" [("a", PrimitiveType Immutable I32)] (PrimitiveType Immutable I32) 
        [ VariableDeclaration "x" (PrimitiveType Immutable I32) (Just (ELiteral (IntLiteral 0))),
          WhileLoop (BinaryOp Lt (Variable "x") (Variable "a")) 
            [VariableReAssignment "x" (BinaryOp Add (Variable "x") (ELiteral (IntLiteral 1)))],
          ReturnStatement (Variable "x")
        ],
    FunctionDeclaration "main" [("a", PrimitiveType Immutable I32)] (PrimitiveType Immutable I32)
        [ReturnStatement $ FunctionCall "dowhile" [Variable "a"]]
    ]

-- input to test 4 functions compilation
testMultipleFunctionAST :: Program
testMultipleFunctionAST = Program 
  [ FunctionDeclaration 
      "add" 
      [("a", PrimitiveType Immutable I32), ("b", PrimitiveType Immutable I32)] 
      (PrimitiveType Immutable I32) 
      [ ReturnStatement $ BinaryOp Add (Variable "a") (Variable "b") ]
  , FunctionDeclaration 
      "sub" 
      [("a", PrimitiveType Immutable I32), ("b", PrimitiveType Immutable I32)] 
      (PrimitiveType Immutable I32) 
      [ ReturnStatement $ BinaryOp Sub (Variable "a") (Variable "b") ]
  , FunctionDeclaration
      "mul"
      [("a", PrimitiveType Immutable I32), ("b", PrimitiveType Immutable I32)]
      (PrimitiveType Immutable I32)
      [ ReturnStatement $ BinaryOp Mul (Variable "a") (Variable "b") ]
  , FunctionDeclaration
      "div"
      [("a", PrimitiveType Immutable I32), ("b", PrimitiveType Immutable I32)]
      (PrimitiveType Immutable I32)
      [ ReturnStatement $ BinaryOp Div (Variable "a") (Variable "b") ]
  ]

-- input to test while loop compilation
testWhileAST :: Program
testWhileAST = Program [
    FunctionDeclaration "dowhile" [("a", PrimitiveType Immutable I32)] (PrimitiveType Immutable I32) 
        [ VariableDeclaration "x" (PrimitiveType Immutable I32) (Just (ELiteral (IntLiteral 0))),
          WhileLoop (BinaryOp Lt (Variable "x") (Variable "a")) 
            [VariableReAssignment "x" (BinaryOp Add (Variable "x") (ELiteral (IntLiteral 1)))],
          ReturnStatement (Variable "x")
        ]
    ]

-- input to test negative variable compilation
testNegNbAST :: Program
testNegNbAST = Program [
    VariableDeclaration "bar" (PrimitiveType Immutable I32) (Just $ ELiteral $ IntLiteral (-256)),
    FunctionDeclaration "addneg" [("a", PrimitiveType Immutable I32)] (PrimitiveType Immutable I32)
        [ReturnStatement $ BinaryOp Add (Variable "a") (Variable "bar")]
    ]

-- input to test global variable compilation
testGlobalAST :: Program
testGlobalAST = Program [
    VariableDeclaration "bar" (PrimitiveType Immutable I32) (Just $ ELiteral $ IntLiteral 256),
    FunctionDeclaration "addglobal" [("a", PrimitiveType Immutable I32)] (PrimitiveType Immutable I32)
        [ReturnStatement $ BinaryOp Add (Variable "a") (Variable "bar")]
    ]

-- input to test basic add function
testBasicFunctionAST :: Program
testBasicFunctionAST = Program [
    FunctionDeclaration "add" [("a", PrimitiveType Immutable I32), ("b", PrimitiveType Immutable I32)] (PrimitiveType Immutable I32)
        [ReturnStatement $ BinaryOp Add (Variable "a") (Variable "b")]
    ]

main :: IO ()
main = do
    printModule filledModule
    writeWasmModule "result.wasm" filledModule
  where
    filledModule = fillWASMModuleFromAST testF64AST
