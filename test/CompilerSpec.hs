{-# LANGUAGE OverloadedStrings #-}

module CompilerSpec (main, spec) where

import Test.Hspec
import Compiler.System.FillModuleData (fillWASMModuleFromAST)
import Parser.Data.Ast
import Compiler.Data.ModuleData

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
main = hspec spec

spec :: Spec
spec = do
  describe "Compiler tests" $ do
    it "compiles a basic add function" $ do
        let ast = testBasicFunctionAST
        let wasmModule = fillWASMModuleFromAST ast
        let expected = WASMModule {types = [TypeSection (FunctionType {funcparams = [ModuleI32,ModuleI32], results = [ModuleI32]})], imports = [], functions = [Function {typeIndex = 0, functionCode = CodeSection {locals = [ModuleI32,ModuleI32], codebody = [ModuleLocalGet 0,ModuleLocalGet 1,ModuleAdd ModuleI32,ModuleReturn]}}], exports = [ExportSection (Export {exportName = "add", exportKind = FunctionExport})], codes = [CodeSection {locals = [ModuleI32,ModuleI32], codebody = [ModuleLocalGet 0,ModuleLocalGet 1,ModuleAdd ModuleI32,ModuleReturn]}], memories = [], globals = []}
        wasmModule `shouldBe` expected
    
    it "compiles a function with global variables" $ do
        let ast = testGlobalAST
        let wasmModule = fillWASMModuleFromAST ast
        let expected = WASMModule {types = [TypeSection (FunctionType {funcparams = [ModuleI32], results = [ModuleI32]})], imports = [], functions = [Function {typeIndex = 0, functionCode = CodeSection {locals = [ModuleI32], codebody = [ModuleLocalGet 0,ModuleGlobalGet 0,ModuleAdd ModuleI32,ModuleReturn]}}], exports = [ExportSection (Export {exportName = "addglobal", exportKind = FunctionExport})], codes = [CodeSection {locals = [ModuleI32], codebody = [ModuleLocalGet 0,ModuleGlobalGet 0,ModuleAdd ModuleI32,ModuleReturn]}], memories = [], globals = [(ModuleI32,ModuleMutable,[ModuleConstI32 256])]}
        wasmModule `shouldBe` expected

    it "compiles a function with negative numbers" $ do
        let ast = testNegNbAST
        let wasmModule = fillWASMModuleFromAST ast
        let expected = WASMModule {types = [TypeSection (FunctionType {funcparams = [ModuleI32], results = [ModuleI32]})], imports = [], functions = [Function {typeIndex = 0, functionCode = CodeSection {locals = [ModuleI32], codebody = [ModuleLocalGet 0,ModuleGlobalGet 0,ModuleAdd ModuleI32,ModuleReturn]}}], exports = [ExportSection (Export {exportName = "addneg", exportKind = FunctionExport})], codes = [CodeSection {locals = [ModuleI32], codebody = [ModuleLocalGet 0,ModuleGlobalGet 0,ModuleAdd ModuleI32,ModuleReturn]}], memories = [], globals = [(ModuleI32,ModuleMutable,[ModuleConstI32 (-256)])]}
        wasmModule `shouldBe` expected
    
    it "compiles a function with while loop" $ do
        let ast = testWhileAST
        let wasmModule = fillWASMModuleFromAST ast
        let expected = WASMModule {types = [TypeSection (FunctionType {funcparams = [ModuleI32], results = [ModuleI32]})], imports = [], functions = [Function {typeIndex = 0, functionCode = CodeSection {locals = [ModuleI32,ModuleI32], codebody = [ModuleBlock Nothing [ModuleLoop Nothing [ModuleLocalGet 1,ModuleLocalGet 0,ModuleLt ModuleI32,ModuleEqz ModuleI32,ModuleBrIf 1,ModuleLocalGet 1,ModuleConstI32 1,ModuleAdd ModuleI32,ModuleLocalSet 1,ModuleBr 0]],ModuleLocalGet 1,ModuleReturn]}}], exports = [ExportSection (Export {exportName = "dowhile", exportKind = FunctionExport})], codes = [CodeSection {locals = [ModuleI32,ModuleI32], codebody = [ModuleBlock Nothing [ModuleLoop Nothing [ModuleLocalGet 1,ModuleLocalGet 0,ModuleLt ModuleI32,ModuleEqz ModuleI32,ModuleBrIf 1,ModuleLocalGet 1,ModuleConstI32 1,ModuleAdd ModuleI32,ModuleLocalSet 1,ModuleBr 0]],ModuleLocalGet 1,ModuleReturn]}], memories = [], globals = []}
        wasmModule `shouldBe` expected
    
    it "compiles multiple functions" $ do
        let ast = testMultipleFunctionAST
        let wasmModule = fillWASMModuleFromAST ast
        let expected = WASMModule {types = [TypeSection (FunctionType {funcparams = [ModuleI32,ModuleI32], results = [ModuleI32]}),TypeSection (FunctionType {funcparams = [ModuleI32,ModuleI32], results = [ModuleI32]}),TypeSection (FunctionType {funcparams = [ModuleI32,ModuleI32], results = [ModuleI32]}),TypeSection (FunctionType {funcparams = [ModuleI32,ModuleI32], results = [ModuleI32]})], imports = [], functions = [Function {typeIndex = 0, functionCode = CodeSection {locals = [ModuleI32,ModuleI32], codebody = [ModuleLocalGet 0,ModuleLocalGet 1,ModuleAdd ModuleI32,ModuleReturn]}},Function {typeIndex = 1, functionCode = CodeSection {locals = [ModuleI32,ModuleI32], codebody = [ModuleLocalGet 0,ModuleLocalGet 1,ModuleSub ModuleI32,ModuleReturn]}},Function {typeIndex = 2, functionCode = CodeSection {locals = [ModuleI32,ModuleI32], codebody = [ModuleLocalGet 0,ModuleLocalGet 1,ModuleMul ModuleI32,ModuleReturn]}},Function {typeIndex = 3, functionCode = CodeSection {locals = [ModuleI32,ModuleI32], codebody = [ModuleLocalGet 0,ModuleLocalGet 1,ModuleDiv ModuleI32,ModuleReturn]}}], exports = [ExportSection (Export {exportName = "add", exportKind = FunctionExport}),ExportSection (Export {exportName = "sub", exportKind = FunctionExport}),ExportSection (Export {exportName = "mul", exportKind = FunctionExport}),ExportSection (Export {exportName = "div", exportKind = FunctionExport})], codes = [CodeSection {locals = [ModuleI32,ModuleI32], codebody = [ModuleLocalGet 0,ModuleLocalGet 1,ModuleAdd ModuleI32,ModuleReturn]},CodeSection {locals = [ModuleI32,ModuleI32], codebody = [ModuleLocalGet 0,ModuleLocalGet 1,ModuleSub ModuleI32,ModuleReturn]},CodeSection {locals = [ModuleI32,ModuleI32], codebody = [ModuleLocalGet 0,ModuleLocalGet 1,ModuleMul ModuleI32,ModuleReturn]},CodeSection {locals = [ModuleI32,ModuleI32], codebody = [ModuleLocalGet 0,ModuleLocalGet 1,ModuleDiv ModuleI32,ModuleReturn]}], memories = [], globals = []}
        wasmModule `shouldBe` expected

    it "compiles a function with function call" $ do
        let ast = testFunctionCallAST
        let wasmModule = fillWASMModuleFromAST ast
        let expected = WASMModule {types = [TypeSection (FunctionType {funcparams = [ModuleI32], results = [ModuleI32]}),TypeSection (FunctionType {funcparams = [ModuleI32], results = [ModuleI32]}),TypeSection (FunctionType {funcparams = [ModuleI32], results = [ModuleI32]})], imports = [], functions = [Function {typeIndex = 0, functionCode = CodeSection {locals = [ModuleI32], codebody = [ModuleLocalGet 0,ModuleGlobalGet 0,ModuleAdd ModuleI32,ModuleReturn]}},Function {typeIndex = 1, functionCode = CodeSection {locals = [ModuleI32,ModuleI32], codebody = [ModuleBlock Nothing [ModuleLoop Nothing [ModuleLocalGet 1,ModuleLocalGet 0,ModuleLt ModuleI32,ModuleEqz ModuleI32,ModuleBrIf 1,ModuleLocalGet 1,ModuleConstI32 1,ModuleAdd ModuleI32,ModuleLocalSet 1,ModuleBr 0]],ModuleLocalGet 1,ModuleReturn]}},Function {typeIndex = 2, functionCode = CodeSection {locals = [ModuleI32], codebody = [ModuleLocalGet 0,ModuleCall 1,ModuleReturn]}}], exports = [ExportSection (Export {exportName = "addneg", exportKind = FunctionExport}),ExportSection (Export {exportName = "dowhile", exportKind = FunctionExport}),ExportSection (Export {exportName = "main", exportKind = FunctionExport})], codes = [CodeSection {locals = [ModuleI32], codebody = [ModuleLocalGet 0,ModuleGlobalGet 0,ModuleAdd ModuleI32,ModuleReturn]},CodeSection {locals = [ModuleI32,ModuleI32], codebody = [ModuleBlock Nothing [ModuleLoop Nothing [ModuleLocalGet 1,ModuleLocalGet 0,ModuleLt ModuleI32,ModuleEqz ModuleI32,ModuleBrIf 1,ModuleLocalGet 1,ModuleConstI32 1,ModuleAdd ModuleI32,ModuleLocalSet 1,ModuleBr 0]],ModuleLocalGet 1,ModuleReturn]},CodeSection {locals = [ModuleI32], codebody = [ModuleLocalGet 0,ModuleCall 1,ModuleReturn]}], memories = [], globals = [(ModuleI32,ModuleMutable,[ModuleConstI32 (-256)])]}
        wasmModule `shouldBe` expected

    it "compiles a function with if statement" $ do
        let ast = testIfAST
        let wasmModule = fillWASMModuleFromAST ast
        let expected = WASMModule {types = [TypeSection (FunctionType {funcparams = [ModuleI32], results = [ModuleI32]})], imports = [], functions = [Function {typeIndex = 0, functionCode = CodeSection {locals = [ModuleI32,ModuleI32], codebody = [ModuleLocalGet 0,ModuleConstI32 0,ModuleEq ModuleI32,ModuleIf Nothing [ModuleConstI32 1,ModuleLocalSet 1] [ModuleConstI32 0,ModuleLocalSet 1],ModuleLocalGet 1,ModuleReturn]}}], exports = [ExportSection (Export {exportName = "doif", exportKind = FunctionExport})], codes = [CodeSection {locals = [ModuleI32,ModuleI32], codebody = [ModuleLocalGet 0,ModuleConstI32 0,ModuleEq ModuleI32,ModuleIf Nothing [ModuleConstI32 1,ModuleLocalSet 1] [ModuleConstI32 0,ModuleLocalSet 1],ModuleLocalGet 1,ModuleReturn]}], memories = [], globals = []}
        wasmModule `shouldBe` expected
    
    it "compiles a function with float" $ do
        let ast = testFloatAST
        let wasmModule = fillWASMModuleFromAST ast
        let expected = WASMModule {types = [TypeSection (FunctionType {funcparams = [ModuleF32,ModuleF32], results = [ModuleF32]})], imports = [], functions = [Function {typeIndex = 0, functionCode = CodeSection {locals = [ModuleF32,ModuleF32], codebody = [ModuleLocalGet 0,ModuleLocalGet 1,ModuleAdd ModuleF32,ModuleReturn]}}], exports = [ExportSection (Export {exportName = "addfloat", exportKind = FunctionExport})], codes = [CodeSection {locals = [ModuleF32,ModuleF32], codebody = [ModuleLocalGet 0,ModuleLocalGet 1,ModuleAdd ModuleF32,ModuleReturn]}], memories = [], globals = []}
        wasmModule `shouldBe` expected
    
    it "compiles a function with I64" $ do
        let ast = testI64AST
        let wasmModule = fillWASMModuleFromAST ast
        let expected = WASMModule {types = [TypeSection (FunctionType {funcparams = [ModuleI64,ModuleI64], results = [ModuleI64]})], imports = [], functions = [Function {typeIndex = 0, functionCode = CodeSection {locals = [ModuleI64,ModuleI64], codebody = [ModuleLocalGet 0,ModuleLocalGet 1,ModuleAdd ModuleI64,ModuleReturn]}}], exports = [ExportSection (Export {exportName = "add64", exportKind = FunctionExport})], codes = [CodeSection {locals = [ModuleI64,ModuleI64], codebody = [ModuleLocalGet 0,ModuleLocalGet 1,ModuleAdd ModuleI64,ModuleReturn]}], memories = [], globals = []}
        wasmModule `shouldBe` expected
    
    it "compiles a function with double" $ do
        let ast = testF64AST
        let wasmModule = fillWASMModuleFromAST ast
        let expected = WASMModule {types = [TypeSection (FunctionType {funcparams = [ModuleF64,ModuleF64], results = [ModuleF64]})], imports = [], functions = [Function {typeIndex = 0, functionCode = CodeSection {locals = [ModuleF64,ModuleF64], codebody = [ModuleLocalGet 0,ModuleLocalGet 1,ModuleAdd ModuleF64,ModuleReturn]}}], exports = [ExportSection (Export {exportName = "addDouble", exportKind = FunctionExport})], codes = [CodeSection {locals = [ModuleF64,ModuleF64], codebody = [ModuleLocalGet 0,ModuleLocalGet 1,ModuleAdd ModuleF64,ModuleReturn]}], memories = [], globals = []}
        wasmModule `shouldBe` expected

