module IRSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Analyzer.IR
import Parser.Data.Ast

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "IR Generation" $ do
    context "Global Collection" $ do
      it "should collect global variables" $ do
        let prog = Program 
              [ VariableDeclaration "x" 
                  (PrimitiveType Immutable I32)
                  (Just (ELiteral (IntLiteral 42)))
              ]
            ir = astToIR prog
        irGlobals ir `shouldBe` [("x", IRInt 32)]

      it "should handle multiple globals" $ do
        let prog = Program 
              [ VariableDeclaration "x" (PrimitiveType Immutable I32) (Just (ELiteral (IntLiteral 1)))
              , VariableDeclaration "y" (PrimitiveType Immutable F32) (Just (ELiteral (FloatLiteral 2.0)))
              ]
            ir = astToIR prog
        irGlobals ir `shouldMatchList` [("x", IRInt 32), ("y", IRFloat 32)]

    context "Function Generation" $ do
      it "should generate IR for simple functions" $ do
        let prog = Program 
              [ FunctionDeclaration "add" 
                  [("a", PrimitiveType Immutable I32),
                   ("b", PrimitiveType Immutable I32)]
                  (PrimitiveType Immutable I32)
                  [ReturnStatement (BinaryOp Add 
                    (Variable "a") 
                    (Variable "b"))]
              ]
            ir = astToIR prog
        length (irFunctions ir) `shouldBe` 1

      it "should handle nested expressions" $ do
        let prog = Program 
              [ FunctionDeclaration "complex" 
                  [("x", PrimitiveType Immutable I32)]
                  (PrimitiveType Immutable I32)
                  [ReturnStatement (BinaryOp Mul 
                    (BinaryOp Add 
                      (Variable "x")
                      (ELiteral (IntLiteral 1)))
                    (ELiteral (IntLiteral 2)))]
              ]
            ir = astToIR prog
        length (irFunctions ir) `shouldBe` 1

    context "Control Flow Generation" $ do
      it "should generate correct IR for while loops" $ do
        let prog = Program 
              [ WhileLoop 
                  (BinaryOp Lt 
                    (Variable "i")
                    (ELiteral (IntLiteral 10)))
                  [VariableReAssignment "i" 
                    (BinaryOp Add 
                      (Variable "i")
                      (ELiteral (IntLiteral 1)))]
              ]
            ir = astToIR prog
            allInstructions = concatMap (irBody . snd) (irFunctions ir)
            hasLabel = any isLabel allInstructions
            isLabel (IRLabel _) = True
            isLabel _ = False
        hasLabel `shouldBe` True

      it "should generate correct IR for if statements" $ do
        let prog = Program 
              [ If (BinaryOp Eq 
                    (Variable "x")
                    (ELiteral (IntLiteral 0)))
                  [ReturnStatement (ELiteral (IntLiteral 1))]
                  (Just [ReturnStatement (ELiteral (IntLiteral 0))])
              ]
            ir = astToIR prog
            allInstructions = concatMap (irBody . snd) (irFunctions ir)
            hasCondition = any isConditional allInstructions
            isConditional (IRCond _ _ _) = True
            isConditional _ = False
        hasCondition `shouldBe` True

  describe "IR Optimization" $ do
    context "Constant Folding" $ do
      it "should fold constant arithmetic" $ do
        let ir = IR 
              { irGlobals = []
              , irFunctions = 
                  [("test", IRFunction 
                    { irParams = []
                    , irLocals = []
                    , irBody = 
                        [ IRBinOp "x" (IRIntLit 2) Add (IRIntLit 3)
                        , IRReturn (Just (IRVar "x"))
                        ]
                    , irReturnType = IRInt 32
                    })]
              , irTypes = []
              }
            optimized = optimizeIR ir
        case irFunctions optimized of
          [("test", IRFunction { irBody = body })] ->
            case body of
              [IRMove "x" (IRIntLit 5), IRReturn (Just (IRVar "x"))] -> 
                True `shouldBe` True
              _ -> expectationFailure "Expected constant to be folded"
          _ -> expectationFailure "Unexpected IR structure after optimization"

      it "should preserve non-constant expressions" $ do
        let ir = IR 
              { irGlobals = []
              , irFunctions = 
                  [("test", IRFunction 
                    { irParams = []
                    , irLocals = []
                    , irBody = 
                        [ IRBinOp "x" (IRVar "a") Add (IRIntLit 3)
                        , IRReturn (Just (IRVar "x"))
                        ]
                    , irReturnType = IRInt 32
                    })]
              , irTypes = []
              }
            optimized = optimizeIR ir
        optimized `shouldBe` ir

    context "Dead Code Elimination" $ do
      it "should eliminate unused variables" $ do
        let ir = IR 
              { irGlobals = []
              , irFunctions = 
                  [("test", IRFunction 
                    { irParams = []
                    , irLocals = []
                    , irBody = 
                        [ IRMove "unused" (IRIntLit 42)
                        , IRMove "used" (IRIntLit 1)
                        , IRReturn (Just (IRVar "used"))
                        ]
                    , irReturnType = IRInt 32
                    })]
              , irTypes = []
              }
            optimized = optimizeIR ir
        length (irBody . snd . head . irFunctions $ optimized) `shouldBe` 2

  describe "Property Tests" $ do
    it "should always generate valid IR for integer literals" $ property $
      \n -> let prog = Program [VariableDeclaration "x" 
                                (PrimitiveType Immutable I32)
                                (Just (ELiteral (IntLiteral n)))]
            in irGlobals (astToIR prog) `shouldBe` [("x", IRInt 32)]

    it "should maintain program validity through optimization" $ property $
      \(NonNegative n) -> 
        let ir = IR 
              { irGlobals = []
              , irFunctions = 
                  [("test", IRFunction 
                    { irParams = []
                    , irLocals = []
                    , irBody = [IRMove "x" (IRIntLit n)]
                    , irReturnType = IRInt 32
                    })]
              , irTypes = []
              }
        in length (irFunctions (optimizeIR ir)) `shouldBe` 1