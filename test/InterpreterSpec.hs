module InterpreterSpec (main, spec) where

import Test.Hspec
import Interpreter.Data.Environment
import Interpreter.System.Operation
import Interpreter.System.Types
import Interpreter.System.Evaluator
import Utils.Data.Result
import Parser.Data.Ast

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Example" $ do

        -- Environment

        it "should change the Env type" $ do
            setLocal (Env [] [] True) `shouldBe` Env [] [] False
            setGlobal (Env [] [] False) `shouldBe` Env [] [] True

        it "should set the Scope of the Env" $ do
            setLocalScope (env True) [eVar "v", eFunc "f"] `shouldBe` Env [] [eVar "v", eFunc "f"] True
            setGlobalScope (env True) [eVar "v", eFunc "f"] `shouldBe` Env [eVar "v", eFunc "f"] [] True

        it "should get an EnvVar from the Env" $ do
            fromEnv (envWithVar "foo") "foo" `shouldBe` Ok (eVar "foo")
            fromEnv (envWithVar "foo") "bar" `shouldBe` Err "bar is undefined"

        it "should add an EnvVar to the Env" $ do
            pushVariable (env True) (VariableDeclaration "foo" iI32 (Just $ iLit 0)) 
                `shouldBe` Ok (envWithVar "foo")
            pushFunction (env True) (FunctionDeclaration "bar" eFuncArgs iI32 eFuncBody)
                `shouldBe` Ok (envWithFunc "bar")
            pushType (env True) (sType "baz")
                `shouldBe` Ok (envWithType "baz")
        
        it "should change the value of a variable" $ do
            assignVar (envWithVar "foo") "foo" (iLit 12) `shouldSatisfy` varHasValue "foo" (iLit 12)
            assignVar (envWithVar "foo") "bar" (iLit 12) `shouldSatisfy` isErr

        -- Types

        it "should wrap an Integer according to a primitive" $ do
            castExpr (env True) (iLit 255) iU8 `shouldSatisfy` isExpr (iLit 255)
            castExpr (env True) (iLit 255) iI8 `shouldSatisfy` isExpr (iLit (-1))
            castExpr (env True) (iLit 256) iU8 `shouldSatisfy` isExpr (iLit 0)
            castExpr (env True) (iLit (-1)) iU8 `shouldSatisfy` isExpr (iLit 255)
            castExpr (env True) (iLit (-1)) iI32 `shouldSatisfy` isExpr (iLit (-1))
            castExpr (env True) (iLit (-1)) iU32 `shouldSatisfy` isExpr (iLit 4294967295)

        it "should evaluate the expression" $ do

            -- Binary Operation
            evalExpr (envWith True [eVarI "foo" 12]) (BinaryOp Add (Variable "foo") (iLit 8))
                `shouldSatisfy` mEvalExprIs (iLit 20)
            evalExpr (envWith True [eVarI "foo" 0]) (BinaryOp Div (iLit 8) (Variable "foo"))
                `shouldBe` Err "Division by zero"

-- Helpers

iLit :: Integer -> Expression
iLit i = ELiteral $ IntLiteral i

iI8 :: Type
iI8 = PrimitiveType Immutable I8

iI32 :: Type
iI32 = PrimitiveType Mutable I32

iU8 :: Type
iU8 = PrimitiveType Immutable U8

iU32 :: Type
iU32 = PrimitiveType Immutable U32

eVar :: String -> EnvVar
eVar n = EVariable n iI32 (ELiteral $ IntLiteral 0)

eVarI :: String -> Integer -> EnvVar
eVarI n i = EVariable n iI32 (iLit i)

eFuncBody :: Body
eFuncBody = [ReturnStatement $ BinaryOp Add (Variable "a") (Variable "b")]

eFuncArgs :: [Field]
eFuncArgs = [("a", iI32), ("b", iI32)]

eFunc :: String -> EnvVar
eFunc n = EFunction n  eFuncArgs iI32 eFuncBody

tEnum :: Type
tEnum = EnumType Immutable (EnumT ["RED", "GREEN", "BLUE"])

sType :: String -> Statement
sType n = TypeDeclaration n tEnum

eType :: String -> EnvVar
eType n = EType n tEnum

envWithType :: String -> Env
envWithType n = envWith True [eType n]

envWithVar :: String -> Env
envWithVar n = envWith True [eVar n]

envWithFunc :: String -> Env
envWithFunc n = envWith True [eFunc n]

envWith :: Bool -> Scope -> Env
envWith True s = Env s [] True
envWith False s = Env [] s False

varHasValue :: String -> Expression -> Result String Env -> Bool
varHasValue n v (Ok e) = case fromEnv e n of
    Ok (EVariable _ _ v') -> v == v'
    _ -> False
varHasValue _ _ _ = False

isExpr :: Expression -> Result String Expression -> Bool
isExpr _ (Err _) = False
isExpr v (Ok v') = v == v'

mEvalExprIs :: Expression -> Result String (Env, Expression) -> Bool
mEvalExprIs _ (Err _) = False
mEvalExprIs v (Ok (_, v')) = v == v'

mEvalEnvIs :: Env -> Result String (Env, Expression) -> Bool
mEvalEnvIs _ (Err _) = False
mEvalEnvIs e (Ok (e', _)) = e == e'

envHasVar :: Env -> String -> Expression -> Bool
envHasVar e n v = case fromEnv e n of
    Ok (EVariable _ _ v') -> v == v'
    _ -> False