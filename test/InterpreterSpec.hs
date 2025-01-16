module InterpreterSpec (main, spec) where

import Test.Hspec
import Interpreter.Data.Environment
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

        it "should represent an Env" $ do
            show (env True) `shouldBe` "\n-- Global Environment --\n\n[Global]\n\n[Local]\n---\n"
            show (env False) `shouldBe` "\n-- Local Environment --\n\n[Global]\n\n[Local]\n---\n"

        it "should compare two EnvVars" $ do
            eVar "foo" `shouldBe` eVar "foo"
            eVar "foo" `shouldNotBe` eVar "bar"
            eFunc "foo" `shouldBe` eFunc "foo"

        it "should show an EnvVar" $ do
            show (eVar "foo") `shouldBe`  "[ var  ] foo: Mutable I32 = 0"
            show (eFunc "foo") `shouldBe` "[ func ] foo (a: Mutable I32, b: Mutable I32) -> Mutable I32"
            show (eType "foo") `shouldBe` "[ type ] foo -> Immutable Enum { RED, GREEN, BLUE }"

        it "should check if an Env is global" $ do
            isGlobal (env True) `shouldBe` True
            isGlobal (env False) `shouldBe` False

        it "should change the Env type" $ do
            setLocal (Env [] [] True) `shouldBe` Env [] [] False
            setGlobal (Env [] [] False) `shouldBe` Env [] [] True
            setIsGlobal False (Env [] [] False) `shouldBe` Env [] [] False
            setIsGlobal True (Env [] [] False) `shouldBe` Env [] [] True

        it "should set the Scope of the Env" $ do
            setLocalScope (env True) [eVar "v", eFunc "f"] `shouldBe` Env [] [eVar "v", eFunc "f"] True
            setGlobalScope (env True) [eVar "v", eFunc "f"] `shouldBe` Env [eVar "v", eFunc "f"] [] True
        
        it "should create an Env from a Scope" $ do
            fromLocal True [eVar "foo"] `shouldBe` Env [] [eVar "foo"] True
            fromGlobal False [eVar "foo"] `shouldBe` Env [eVar "foo"] [] False

        it "should get elements from all scopes of an Env" $ do
            envAll (envWithStuff True) `shouldBe` [eFunc "foo", eType "bar", eVar "baz"]
            envAllNames (envWithStuff True) `shouldBe` ["foo", "bar", "baz"]

        it "should get an EnvVar from the Env" $ do
            fromEnv (envWithVar "foo") "foo" `shouldBe` Ok (eVar "foo")
            fromEnv (envWithVar "foo") "bar" `shouldBe` Err "bar is undefined"
            fromEnv (envWithStuff True) "baz" `shouldBe` Ok (eVar "baz")

        it "should add an EnvVar to the Env" $ do
            pushVariable (env True) (VariableDeclaration "foo" iI32 (Just $ iLit 0)) 
                `shouldBe` Ok (envWithVar "foo")
            pushVariable (env True) (VariableDeclaration "foo" iI32 Nothing) 
                `shouldBe` Err "Variable foo must have a value to be added to env"
            pushVariable (envWithVar "foo") (VariableDeclaration "foo" iI32 (Just $ iLit 0)) 
                `shouldBe` Err "foo redefined"
            pushVariable (envWithVar "foo") (FunctionDeclaration "bar" eFuncArgs iI32 eFuncBody) 
                `shouldBe` Err "Bad variable type (FunctionDeclaration \"bar\" [(\"a\",Mutable I32),(\"b\",Mutable I32)] Mutable I32 [ReturnStatement \"a\" Add \"b\"])"
            pushFunction (env True) (FunctionDeclaration "bar" eFuncArgs iI32 eFuncBody)
                `shouldBe` Ok (envWithFunc "bar")
            pushFunction (envWithFunc "bar") (FunctionDeclaration "bar" eFuncArgs iI32 eFuncBody)
                `shouldSatisfy` isErr
            pushType (env True) (sType "baz")
                `shouldBe` Ok (envWithType "baz")
            pushType (envWithType "baz") (sType "baz")
                `shouldSatisfy` isErr

        it "should change the value of a variable" $ do
            assignVar (envWithVar "foo") "foo" (iLit 12) `shouldSatisfy` varHasValue "foo" (iLit 12)
            assignVar (envWithVar "foo") "bar" (iLit 12) `shouldBe` Err "bar is undefined" 

        -- Types

        it "should clamp an Integer according to a primitive" $ do
            castExpr (env True) (iLit 255) iI8 `shouldSatisfy` isExpr (iLit 127)
            castExpr (env True) (iLit 32769) iI16 `shouldSatisfy` isExpr (iLit 32767)
            castExpr (env True) (iLit 2147483649) iI32 `shouldSatisfy` isExpr (iLit 2147483647)
            castExpr (env True) (iLit 9223372036854775810) iI64 `shouldSatisfy` isExpr (iLit 9223372036854775807)
            castExpr (env True) (iLit 257) iU8 `shouldSatisfy` isExpr (iLit 255)
            castExpr (env True) (iLit 65536) iU16 `shouldSatisfy` isExpr (iLit 65535)
            castExpr (env True) (iLit 4294967298) iU32 `shouldSatisfy` isExpr (iLit 4294967295)
            castExpr (env True) (iLit 18446744073709551618) iU64 `shouldSatisfy` isExpr (iLit 18446744073709551615)
            castExpr (env True) (fLit 1.3) fF64 `shouldSatisfy` isExpr (fLit 1.3)

        it "should return a default value for the given type" $ do
            defaultExpr (env True) (PrimitiveType Immutable I32) `shouldBe` Ok (iLit 0)
            defaultExpr (env True) (PrimitiveType Immutable U8) `shouldBe` Ok (iLit 0)
            defaultExpr (env True) (PrimitiveType Immutable F32) `shouldBe` Ok (fLit 0.0)
            defaultExpr (env True) (PrimitiveType Immutable F64) `shouldBe` Ok (fLit 0.0)
            defaultExpr (envWith True [EType "MyF" fF32]) (CustomType Immutable "MyF")
                `shouldBe` Ok (fLit 0.0)
            defaultExpr (env True) (ArrayType Immutable (Array 0 iI32))
                `shouldSatisfy` isErr

        -- Expressions

        it "should evaluate the variable expression" $ do
            evalExpr (envWith True [eVarI "foo" 3]) (Variable "foo")
                `shouldSatisfy` evalExprIs (iLit 3)
            evalExpr (envWith True [eVarI "foo" 3]) (Variable "bar")
                `shouldSatisfy` isErr

        it "should evaluate the literal expression" $ do
            evalExpr (env True) (ELiteral $ IntLiteral 42)
                `shouldSatisfy` evalExprIs (iLit 42)
            evalExpr (env True) (ELiteral $ FloatLiteral 3.14)
                `shouldSatisfy` evalExprIs (fLit 3.14)

        it "should evaluate the binary operation" $ do
            evalExpr (envWith True [eVarI "foo" 12]) (BinaryOp Add (Variable "foo") (iLit 8))
                `shouldSatisfy` evalExprIs (iLit 20)
            evalExpr (env True) (BinaryOp Sub (iLit 10) (iLit 3))
                `shouldSatisfy` evalExprIs (iLit 7)
            evalExpr (env True) (BinaryOp Mul (iLit 10) (iLit 3))
                `shouldSatisfy` evalExprIs (iLit 30)
            evalExpr (env True) (BinaryOp Eq (iLit 10) (iLit 3))
                `shouldSatisfy` evalExprIs (iLit 0)
            evalExpr (env True) (BinaryOp Neq (iLit 10) (iLit 3))
                `shouldSatisfy` evalExprIs (iLit 1)
            evalExpr (env True) (BinaryOp Lt (iLit 10) (iLit 3))
                `shouldSatisfy` evalExprIs (iLit 0)
            evalExpr (env True) (BinaryOp Gt (iLit 10) (iLit 3))
                `shouldSatisfy` evalExprIs (iLit 1)
            evalExpr (env True) (BinaryOp Le (iLit 10) (iLit 10))
                `shouldSatisfy` evalExprIs (iLit 1)
            evalExpr (env True) (BinaryOp Ge (iLit 10) (iLit 3))
                `shouldSatisfy` evalExprIs (iLit 1)
            evalExpr (env True) (BinaryOp And (iLit 10) (iLit 3))
                `shouldSatisfy` evalExprIs (iLit 1)
            evalExpr (env True) (BinaryOp And (iLit 0) (iLit 3))
                `shouldSatisfy` evalExprIs (iLit 0)
            evalExpr (env True) (BinaryOp Or (iLit 0) (iLit 3))
                `shouldSatisfy` evalExprIs (iLit 1)
            evalExpr (env True) (BinaryOp Mod (iLit 10) (iLit 2))
                `shouldSatisfy` evalExprIs (iLit 0)
            evalExpr (env True) (BinaryOp Mod (iLit 13) (iLit 2))
                `shouldSatisfy` evalExprIs (iLit 1)
            evalExpr (env True) (BinaryOp BitAnd (iLit 13) (iLit 7))
                `shouldSatisfy` evalExprIs (iLit 5)
            evalExpr (env True) (BinaryOp BitOr (iLit 13) (iLit 7))
                `shouldSatisfy` evalExprIs (iLit 15)
            evalExpr (env True) (BinaryOp BitXor (iLit 13) (iLit 7))
                `shouldSatisfy` evalExprIs (iLit 10)
            evalExpr (envWith True [eVarI "foo" 0]) (BinaryOp Div (iLit 8) (Variable "foo"))
                `shouldBe` Err "Division by zero"
            evalExpr (envWith True [eVarI "baz" 2]) (BinaryOp Shl (Variable "baz") (iLit 8))
                `shouldSatisfy` evalExprIs (iLit 512)
            evalExpr (envWith True [eVarI "baz" 1024]) (BinaryOp Shr (Variable "baz") (iLit 2))
                `shouldSatisfy` evalExprIs (iLit 256)
            evalExpr (envWith True [eVarF "foo" 7.7]) (BinaryOp Add (Variable "foo") (fLit 3.3))
                `shouldSatisfy` evalExprIs (fLit 11.0)
            evalExpr (envWith True [eVarI "bar" 3]) (BinaryOp Div (Variable "bar") (iLit 2))
                `shouldSatisfy` evalExprIs (fLit 1.5)

        it "should evaluate the unary operation" $ do
            evalExpr (env True) (UnaryOp Negate (iLit 8))
                `shouldSatisfy` evalExprIs (iLit 0)
            evalExpr (env True) (UnaryOp Negative (iLit 12))
                `shouldSatisfy` evalExprIs (iLit (-12))
            evalExpr (env True) (UnaryOp BitNot (iLit 24))
                `shouldSatisfy` evalExprIs (iLit (-25))
            evalExpr (env True) (UnaryOp Negate (fLit 24.5))
                `shouldSatisfy` evalExprIs (fLit 0.0)
            evalExpr (env True) (UnaryOp Negate (fLit 0.0))
                `shouldSatisfy` evalExprIs (fLit 1.0)

        it "should evaluate the parenthesis expression" $ do
            evalExpr (envWith True [eVarI "baz" 13]) (Parenthesis (Variable "baz"))
                `shouldSatisfy` evalExprIs (iLit 13)
        
        it "should evaluate the function call expression" $ do
            evalExpr (envWithFunc "foo") (FunctionCall "foo" [iLit 3, iLit 4]) 
                `shouldSatisfy` evalExprIs (iLit 7)
            evalExpr (envWith True [eDefFunc "add", eVarI "foo" 42]) (FunctionCall "add" [iLit 84])
                `shouldSatisfy` evalExprIs (iLit 84)
            evalExpr (envWithFunc "foo") (FunctionCall "foo" [])
                `shouldSatisfy` isErr

        -- Statements

        it "should evaluate the variable declaration statement" $ do
            evalStatement (env True) (VariableDeclaration "foo" iI32 (Just $ iLit 42)) 
                `shouldBe` Ok (Env [eVarI "foo" 42] [] True, Nothing)
            evalStatement (env True) (VariableDeclaration "foo" iI32 Nothing)
                `shouldBe` Ok (Env [eVarI "foo" 0] [] True, Nothing)
            evalStatement (envWithVar "foo") (VariableDeclaration "foo" iI32 (Just $ iLit 42))
                `shouldSatisfy` isErr

        it "should evaluate the function declaration statement" $ do
            evalStatement (env True) (FunctionDeclaration "foo" eFuncArgs iI32 eFuncBody)
                `shouldBe` Ok (envWithFunc "foo", Nothing)
            evalStatement (envWithFunc "foo") (FunctionDeclaration "foo" eFuncArgs iI32 eFuncBody)
                `shouldSatisfy` isErr

        it "should evaluation the while loop" $ do
            evalStatement (envWith True [eVar "foo", eVarI "bar" 10, eVar "baz"]) eWhile
                `shouldBe` Ok (envWith True [eVarI "foo" 10, eVarI "bar" 10, eVarI "baz" 20], Nothing)

        it "should evaluate the if statement" $ do
            evalStatement (envWith True [eVarI "foo" 10, eVarI "bar" 20, eVarI "baz" 0]) eIf
                `shouldBe` Ok (envWith True [eVarI "foo" 11, eVarI "bar" 20, eVarI "baz" 2], Nothing)
            evalStatement (envWith True [eVarI "foo" 20, eVarI "bar" 10, eVarI "baz" 0]) eIf
                `shouldBe` Ok (envWith True [eVarI "foo" (-1), eVarI "bar" 10, eVarI "baz" 0], Nothing)

        it "should evaluate the type declaration statement" $ do
            evalStatement (env True) (TypeDeclaration "foo" tEnum)
                `shouldBe` Ok (envWithType "foo", Nothing)
            evalStatement (envWithType "foo") (TypeDeclaration "foo" tEnum)
                `shouldSatisfy` isErr
        
        it "should evaluate the return statement" $ do
            evalStatement (env True) (ReturnStatement $ iLit 42)
                `shouldSatisfy` mEvalExprIs (Just $ iLit 42)
            evalStatement (env True) (ReturnStatement $ Variable "foo")
                `shouldSatisfy` isErr
        
        it "should evaluate the standalone function call" $ do
            evalStatement (envWithFunc "foo") (StandaloneFunctionCall "foo" [iLit 3, iLit 4])
                `shouldBe` Ok (envWithFunc "foo", Nothing)
            evalStatement (envWithFunc "foo") (StandaloneFunctionCall "bar" [iLit 3, iLit 4])
                `shouldSatisfy` isErr
        
        it "should evaluate the variable reassignment statement" $ do
            evalStatement (envWithVar "foo") (VariableReAssignment "foo" (iLit 42))
                `shouldBe` Ok (envWith True [eVarI "foo" 42], Nothing)

        -- Program

        it "should evaluate the program" $ do
            evalProg prog 
                `shouldBe` Ok (envWith True [eVarI "baz" 66, eVarI "bar" 12, eVarI "foo" 12] , iLit 0)

-- Helpers

iLit :: Integer -> Expression
iLit i = ELiteral $ IntLiteral i

fLit :: Double -> Expression
fLit i = ELiteral $ FloatLiteral i

iI8 :: Type
iI8 = PrimitiveType Immutable I8

iI16 :: Type
iI16 = PrimitiveType Immutable I16

iI32 :: Type
iI32 = PrimitiveType Mutable I32

iI64 :: Type
iI64 = PrimitiveType Mutable I64

fF32 :: Type
fF32 = PrimitiveType Mutable F32

fF64 :: Type
fF64 = PrimitiveType Mutable F64

iU8 :: Type
iU8 = PrimitiveType Immutable U8

iU16 :: Type
iU16 = PrimitiveType Immutable U16

iU32 :: Type
iU32 = PrimitiveType Immutable U32

iU64 :: Type
iU64 = PrimitiveType Immutable U64

eVar :: String -> EnvVar
eVar n = EVariable n iI32 (ELiteral $ IntLiteral 0)

eVarI :: String -> Integer -> EnvVar
eVarI n i = EVariable n iI32 (iLit i)

eVarF :: String -> Double -> EnvVar
eVarF n f = EVariable n fF32 (fLit f)

eDefFunc :: String -> EnvVar
eDefFunc n = EFunction n [("foo", iI32)] iI32 [ ReturnStatement (Variable "foo") ]

eFuncBody :: Body
eFuncBody = [ReturnStatement $ BinaryOp Add (Variable "a") (Variable "b")]

eFuncArgs :: [Field]
eFuncArgs = [("a", iI32), ("b", iI32)]

eBody2 :: Body
eBody2 = [VariableReAssignment "foo" (iLit (-1))]

eBody :: Body
eBody = [ VariableReAssignment "foo" (BinaryOp Add (Variable "foo") (iLit 1)) 
             , VariableReAssignment "baz" (BinaryOp Add (Variable "baz") (iLit 2)) ]

eCond :: Expression
eCond = BinaryOp Lt (Variable "foo") (Variable "bar")

eIf :: Statement
eIf = If eCond eBody (Just eBody2)

eWhile :: Statement
eWhile = WhileLoop eCond eBody

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

envWithStuff :: Bool -> Env
envWithStuff = Env [eFunc "foo", eType "bar"] [eVar "baz"]

varHasValue :: String -> Expression -> Result String Env -> Bool
varHasValue n v (Ok e) = case fromEnv e n of
    Ok (EVariable _ _ v') -> v == v'
    _ -> False
varHasValue _ _ _ = False

isExpr :: Expression -> Result String Expression -> Bool
isExpr _ (Err _) = False
isExpr v (Ok v') = v == v'

evalExprIs :: Expression -> Result String (Env, Expression) -> Bool
evalExprIs _ (Err _) = False
evalExprIs v (Ok (_, v')) = v == v'

mEvalExprIs :: Maybe Expression -> Result String (Env, Maybe Expression) -> Bool
mEvalExprIs _ (Err _) = False
mEvalExprIs v (Ok (_, v')) = v == v'

prog :: Program
prog = Program [ VariableDeclaration "foo" iI32 Nothing
               , VariableDeclaration "bar" iI32 Nothing
               , VariableDeclaration "baz" iI32 (Just $ iLit 42)
               , VariableReAssignment "bar" (iLit 12)
               , eWhile ]