module InterpreterSpec (main, spec) where

import Test.Hspec
import Interpreter.Data.Environment
import Interpreter.System.Types
import Interpreter.System.Evaluator
import Utils.Data.Result
import Parser.Data.Ast
import Interpreter.System.Optimizer (optimizeProg)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Example" $ do

        -- Environment

        it "should represent an Env" $ do
            show env `shouldBe` "\n-- Environment --\n---\n"
            show env `shouldBe` "\n-- Environment --\n---\n"

        it "should compare two EnvVars" $ do
            eVar "foo" `shouldBe` eVar "foo"
            eVar "foo" `shouldNotBe` eVar "bar"
            eFunc "foo" `shouldBe` eFunc "foo"

        it "should show an EnvVar" $ do
            show (eVar "foo") `shouldBe`  "[ var  ] foo: Mutable I32 = 0"
            show (eFunc "foo") `shouldBe` "[ func ] foo (a: Immutable I32, b: Immutable I32) -> Immutable I32"
            show (eType "foo") `shouldBe` "[ type ] foo -> Immutable Enum { RED, GREEN, BLUE }"

        -- it "should add a scope to an Env" $ do
        --     pushScope (fromScope scope) scope1 `shouldBe` (Env [scope1, scope])
        --         where
        --             scope = [eVarI 42]
        --             scope2 = [eVarI 10]

        -- it "should remove a Scope of the Env" $ do
        --     popScope (Env [scope, scope1]) scope1 `shouldBe` (Env [scope1])
        --         where
        --             scope = [eVarI 42]
        --             scope2 = [eVarI 10]

        it "should get elements from all scopes of an Env" $ do
            envAll envWithStuff `shouldBe` [eFunc "foo", eType "bar", eVar "baz"]
            envAllNames envWithStuff `shouldBe` ["foo", "bar", "baz"]

        it "should get an EnvVar from the Env" $ do
            fromEnv (envWithVar "foo") "foo" `shouldBe` Ok (eVar "foo")
            fromEnv (envWithVar "foo") "bar" `shouldBe` Err "bar is undefined"
            fromEnv envWithStuff "baz" `shouldBe` Ok (eVar "baz")

        it "should add an EnvVar to the Env" $ do
            pushVariable env (VariableDeclaration "foo" iI32M (Just $ iLit 0))
                `shouldBe` Ok (envWithVar "foo")
            pushVariable env (VariableDeclaration "foo" iI32 Nothing)
                `shouldBe` Err "Variable foo must have a value to be added to env"
            pushVariable (envWithVar "foo") (VariableDeclaration "foo" iI32 (Just $ iLit 0))
                `shouldBe` Err "foo redefined"
            pushVariable (envWithVar "foo") (FunctionDeclaration "bar" eFuncArgs iI32 eFuncBody)
                `shouldBe` Err "Bad variable type (FunctionDeclaration \"bar\" [(\"a\",Immutable I32),(\"b\",Immutable I32)] Immutable I32 [ReturnStatement \"a\" Add \"b\"])"
            pushFunction env (FunctionDeclaration "bar" eFuncArgs iI32 eFuncBody)
                `shouldBe` Ok (envWithFunc "bar")
            pushFunction (envWithFunc "bar") (FunctionDeclaration "bar" eFuncArgs iI32 eFuncBody)
                `shouldSatisfy` isErr
            pushType env (sType "baz")
                `shouldBe` Ok (envWithType "baz")
            pushType (envWithType "baz") (sType "baz")
                `shouldSatisfy` isErr

        it "should change the value of a variable" $ do
            assignVar (envWithVar "foo") "foo" (iLit 12) `shouldSatisfy` varHasValue "foo" (iLit 12)
            assignVar (envWithVar "foo") "bar" (iLit 12) `shouldBe` Err "bar is undefined"

        -- Types

        it "should wrap an Integer according to a primitive" $ do
            castExpr env (iLit 255) iI8 `shouldSatisfy` isExpr (iLit (-1))
            castExpr env (iLit 32769) iI16 `shouldSatisfy` isExpr (iLit (-32767))
            castExpr env (iLit 2147483648) iI32 `shouldSatisfy` isExpr (iLit (-2147483648))
            castExpr env (iLit 9223372036854775810) iI64 `shouldSatisfy` isExpr (iLit (-9223372036854775806))
            castExpr env (iLit 257) iU8 `shouldSatisfy` isExpr (iLit 1)
            castExpr env (iLit 65536) iU16 `shouldSatisfy` isExpr (iLit 0)
            castExpr env (iLit 4294967298) iU32 `shouldSatisfy` isExpr (iLit 2)
            castExpr env (iLit 18446744073709551618) iU64 `shouldSatisfy` isExpr (iLit 2)
            castExpr env (fLit 1.3) fF64 `shouldSatisfy` isExpr (fLit 1.3)

        it "should return a default value for the given type" $ do
            defaultExpr env (PrimitiveType Immutable I32) `shouldBe` Ok (iLit 0)
            defaultExpr env (PrimitiveType Immutable U8) `shouldBe` Ok (iLit 0)
            defaultExpr env (PrimitiveType Immutable F32) `shouldBe` Ok (fLit 0.0)
            defaultExpr env (PrimitiveType Immutable F64) `shouldBe` Ok (fLit 0.0)
            defaultExpr (envWith [EType "MyF" fF32]) (CustomType Immutable "MyF")
                `shouldBe` Ok (fLit 0.0)
            defaultExpr env (ArrayType Immutable (Array 0 iI32))
                `shouldSatisfy` isErr

        -- Expressions

        it "should evaluate the variable expression" $ do
            evalExpr (envWith [eVarI "foo" 3]) (Variable "foo")
                `shouldSatisfy` evalExprIs (iLit 3)
            evalExpr (envWith [eVarI "foo" 3]) (Variable "bar")
                `shouldSatisfy` isErr

        it "should evaluate the literal expression" $ do
            evalExpr env (ELiteral $ IntLiteral 42)
                `shouldSatisfy` evalExprIs (iLit 42)
            evalExpr env (ELiteral $ FloatLiteral 3.14)
                `shouldSatisfy` evalExprIs (fLit 3.14)

        it "should evaluate the binary operation" $ do
            evalExpr (envWith [eVarI "foo" 12]) (BinaryOp Add (Variable "foo") (iLit 8))
                `shouldSatisfy` evalExprIs (iLit 20)
            evalExpr env (BinaryOp Sub (iLit 10) (iLit 3))
                `shouldSatisfy` evalExprIs (iLit 7)
            evalExpr env (BinaryOp Mul (iLit 10) (iLit 3))
                `shouldSatisfy` evalExprIs (iLit 30)
            evalExpr env (BinaryOp Eq (iLit 10) (iLit 3))
                `shouldSatisfy` evalExprIs (iLit 0)
            evalExpr env (BinaryOp Neq (iLit 10) (iLit 3))
                `shouldSatisfy` evalExprIs (iLit 1)
            evalExpr env (BinaryOp Lt (iLit 10) (iLit 3))
                `shouldSatisfy` evalExprIs (iLit 0)
            evalExpr env (BinaryOp Gt (iLit 10) (iLit 3))
                `shouldSatisfy` evalExprIs (iLit 1)
            evalExpr env (BinaryOp Le (iLit 10) (iLit 10))
                `shouldSatisfy` evalExprIs (iLit 1)
            evalExpr env (BinaryOp Ge (iLit 10) (iLit 3))
                `shouldSatisfy` evalExprIs (iLit 1)
            evalExpr env (BinaryOp And (iLit 10) (iLit 3))
                `shouldSatisfy` evalExprIs (iLit 1)
            evalExpr env (BinaryOp And (iLit 0) (iLit 3))
                `shouldSatisfy` evalExprIs (iLit 0)
            evalExpr env (BinaryOp Or (iLit 0) (iLit 3))
                `shouldSatisfy` evalExprIs (iLit 1)
            evalExpr env (BinaryOp Mod (iLit 10) (iLit 2))
                `shouldSatisfy` evalExprIs (iLit 0)
            evalExpr env (BinaryOp Mod (iLit 13) (iLit 2))
                `shouldSatisfy` evalExprIs (iLit 1)
            evalExpr env (BinaryOp BitAnd (iLit 13) (iLit 7))
                `shouldSatisfy` evalExprIs (iLit 5)
            evalExpr env (BinaryOp BitOr (iLit 13) (iLit 7))
                `shouldSatisfy` evalExprIs (iLit 15)
            evalExpr env (BinaryOp BitXor (iLit 13) (iLit 7))
                `shouldSatisfy` evalExprIs (iLit 10)
            evalExpr (envWith [eVarI "baz" 2]) (BinaryOp Shl (Variable "baz") (iLit 8))
                `shouldSatisfy` evalExprIs (iLit 512)
            evalExpr (envWith [eVarI "baz" 1024]) (BinaryOp Shr (Variable "baz") (iLit 2))
                `shouldSatisfy` evalExprIs (iLit 256)
            evalExpr (envWith [eVarF "foo" 7.7]) (BinaryOp Add (Variable "foo") (fLit 3.3))
                `shouldSatisfy` evalExprIs (fLit 11.0)
            evalExpr (envWith [eVarI "bar" 3]) (BinaryOp Div (Variable "bar") (iLit 2))
                `shouldSatisfy` evalExprIs (iLit 1)
            -- evalExpr (envWith [eVarI "foo" 0]) (BinaryOp Div (iLit 8) (Variable "foo"))
            --     `shouldBe` Err "Division by zero"

        it "should evaluate the unary operation" $ do
            evalExpr env (UnaryOp Negate (iLit 8))
                `shouldSatisfy` evalExprIs (iLit 0)
            evalExpr env (UnaryOp Negative (iLit 12))
                `shouldSatisfy` evalExprIs (iLit (-12))
            evalExpr env (UnaryOp BitNot (iLit 24))
                `shouldSatisfy` evalExprIs (iLit (-25))
            evalExpr env (UnaryOp Negate (fLit 24.5))
                `shouldSatisfy` evalExprIs (fLit 0.0)
            evalExpr env (UnaryOp Negate (fLit 0.0))
                `shouldSatisfy` evalExprIs (fLit 1.0)

        it "should evaluate the parenthesis expression" $ do
            evalExpr (envWith [eVarI "baz" 13]) (Parenthesis (Variable "baz"))
                `shouldSatisfy` evalExprIs (iLit 13)

        it "should evaluate the function call expression" $ do
            evalExpr (envWithFunc "foo") (FunctionCall "foo" [iLit 3, iLit 4])
                `shouldSatisfy` evalExprIs (iLit 7)
            evalExpr (envWith [eDefFunc "add", eVarI "foo" 42]) (FunctionCall "add" [iLit 84])
                `shouldSatisfy` evalExprIs (iLit 84)
            evalExpr (envWithFunc "foo") (FunctionCall "foo" [])
                `shouldSatisfy` isErr

        -- Statements

        it "should evaluate the variable declaration statement" $ do
            evalStatement env (VariableDeclaration "foo" iI32M (Just $ iLit 42))
                `shouldBe` Ok (envWith [eVarI "foo" 42], Nothing)
            evalStatement env (VariableDeclaration "foo" iI32M Nothing)
                `shouldBe` Ok (envWith [eVarI "foo" 0], Nothing)
            evalStatement (envWithVar "foo") (VariableDeclaration "foo" iI32 (Just $ iLit 42))
                `shouldSatisfy` isErr

        it "should evaluate the function declaration statement" $ do
            evalStatement env (FunctionDeclaration "foo" eFuncArgs iI32 eFuncBody)
                `shouldBe` Ok (envWithFunc "foo", Nothing)
            evalStatement (envWithFunc "foo") (FunctionDeclaration "foo" eFuncArgs iI32 eFuncBody)
                `shouldSatisfy` isErr

        it "should evaluation the while loop" $ do
            evalStatement (envWith [eVar "foo", eVarI "bar" 10, eVar "baz"]) eWhile
                `shouldBe` Ok (envWith [eVarI "foo" 10, eVarI "bar" 10, eVarI "baz" 20], Nothing)

        it "should evaluate the if statement" $ do
            evalStatement (envWith [eVarI "foo" 10, eVarI "bar" 20, eVarI "baz" 0]) eIf
                `shouldBe` Ok (envWith [eVarI "foo" 11, eVarI "bar" 20, eVarI "baz" 2], Nothing)
            evalStatement (envWith [eVarI "foo" 20, eVarI "bar" 10, eVarI "baz" 0]) eIf
                `shouldBe` Ok (envWith [eVarI "foo" (-1), eVarI "bar" 10, eVarI "baz" 0], Nothing)

        it "should evaluate the type declaration statement" $ do
            evalStatement env (TypeDeclaration "foo" tEnum)
                `shouldBe` Ok (envWithType "foo", Nothing)
            evalStatement (envWithType "foo") (TypeDeclaration "foo" tEnum)
                `shouldSatisfy` isErr

        it "should evaluate the return statement" $ do
            evalStatement env (ReturnStatement $ iLit 42)
                `shouldSatisfy` mEvalExprIs (Just $ iLit 42)
            evalStatement env (ReturnStatement $ Variable "foo")
                `shouldSatisfy` isErr

        it "should evaluate the standalone function call" $ do
            evalStatement (envWithFunc "foo") (StandaloneFunctionCall "foo" [iLit 3, iLit 4])
                `shouldBe` Ok (envWithFunc "foo", Nothing)
            evalStatement (envWithFunc "foo") (StandaloneFunctionCall "bar" [iLit 3, iLit 4])
                `shouldSatisfy` isErr

        it "should evaluate the variable reassignment statement" $ do
            evalStatement (envWithVar "foo") (VariableReAssignment "foo" (iLit 42))
                `shouldBe` Ok (envWith [eVarI "foo" 42], Nothing)

        -- Program

        it "should evaluate the program" $ do
            evalProg prog
                `shouldBe` Ok (envWith [eVarI "baz" 66, eVarI "bar" 12, eVarI "foo" 12] , iLit 0)

        -- Optimizer

        it "should fold constant variables" $ do
            Program [ VariableDeclaration "foo" (PrimitiveType Immutable I32) (Just $ iLit 42)
                    , ReturnStatement (Variable "foo") ]
                `shouldSatisfy`
                    optimizeChange [ ReturnStatement (iLit 42) ]

            Program [ VariableDeclaration "foo" (PrimitiveType Immutable I32) (Just $ iLit 42)
                    , VariableDeclaration "bar" (PrimitiveType Mutable I32) (Just $ iLit 8)
                    , ReturnStatement (BinaryOp Add (Variable "foo") (Variable "bar")) ]
                `shouldSatisfy`
                    optimizeChange [ VariableDeclaration "bar" (PrimitiveType Mutable I32) (Just $ iLit 8)
                                   , ReturnStatement (BinaryOp Add (iLit 42) (Variable "bar")) ]

            Program [ VariableDeclaration "foo" (PrimitiveType Mutable I32) (Just $ iLit 42)
                    , ReturnStatement (Variable "foo") ]
                `shouldSatisfy`
                    optimizeNoChange

        it "should pre-evaluate a constant binary operation" $ do
            Program [ ReturnStatement (BinaryOp Add (iLit 44) (iLit 16)) ]
                `shouldSatisfy`
                    optimizeChange [ ReturnStatement (iLit 60) ]

            Program [ VariableDeclaration "foo" (PrimitiveType Immutable I32) (Just $ iLit 10)
                    , ReturnStatement (BinaryOp Add (Variable "foo") (iLit 5))]
                `shouldSatisfy`
                    optimizeChange [ReturnStatement (iLit 15)]

            Program [ VariableDeclaration "foo" (PrimitiveType Mutable I32) (Just $ iLit 42)
                    , VariableDeclaration "bar" (PrimitiveType Mutable I32) (Just $ iLit 8)
                    , ReturnStatement (BinaryOp Add (Variable "foo") (Variable "bar")) ]
                `shouldSatisfy`
                    optimizeNoChange
            
        it "should unpack a constant IF statement" $ do
            Program [ If (iLit 1) [ReturnStatement (iLit 42)] (Just [ReturnStatement (iLit 84)]) ]
                `shouldSatisfy`
                    optimizeChange [ReturnStatement (iLit 42)]

            Program [ If (iLit 0) [ReturnStatement (iLit 42)] (Just [ReturnStatement (iLit 84)]) ]
                `shouldSatisfy`
                    optimizeChange [ReturnStatement (iLit 84)]
            
            Program [ If (iLit 0) [ReturnStatement (iLit 42)] (Just [
                        If (iLit 1) [ReturnStatement (iLit 1)] (Just [ReturnStatement (iLit 84)])]) 
                    ]
                `shouldSatisfy`
                    optimizeChange [ReturnStatement (iLit 1)]
            
            Program [ If (Variable "foo") [ReturnStatement (iLit 42)] (Just [ReturnStatement (iLit 84)]) ]
                `shouldSatisfy`
                    optimizeChange [ If (Variable "foo") [ReturnStatement (iLit 42)] Nothing
                                   , ReturnStatement (iLit 84) ]

            Program [ If (Variable "foo") [StandaloneFunctionCall "bar" []] (Just [StandaloneFunctionCall "baz" []]) ]
                `shouldSatisfy`
                    optimizeNoChange

        it "should remove unreachable code after a return" $ do
            Program [ ReturnStatement (BinaryOp Add (Variable "foo") (Variable "bar")) ]
                `shouldSatisfy`
                    optimizeNoChange
            
            Program [ VariableDeclaration "foo" (PrimitiveType Mutable I32) (Just $ iLit 42)
                    , VariableDeclaration "bar" (PrimitiveType Mutable I32) (Just $ iLit 8)
                    , ReturnStatement (BinaryOp Add (Variable "foo") (Variable "bar"))
                    , VariableDeclaration "baz" (PrimitiveType Mutable I32) (Just $ iLit 14)
                    , ReturnStatement (Variable "baz") ]
                `shouldSatisfy`
                    optimizeChange [ VariableDeclaration "foo" (PrimitiveType Mutable I32) (Just $ iLit 42)
                                   , VariableDeclaration "bar" (PrimitiveType Mutable I32) (Just $ iLit 8)
                                   , ReturnStatement (BinaryOp Add (Variable "foo") (Variable "bar")) ]

        it "should substitue a bitwise not with a bitwise xor" $ do
            Program [ ReturnStatement (UnaryOp BitNot (Variable "foo")) ]
                `shouldSatisfy`
                    optimizeChange [ ReturnStatement (BinaryOp BitXor (iLit (-1)) (Variable "foo")) ]

        it "should inline a function content" $ do
            Program [ FunctionDeclaration "foo" eFuncArgs iI32 eFuncBody 
                    , ReturnStatement (FunctionCall "foo" [iLit 13, iLit 32]) ]
                `shouldSatisfy`
                    optimizeChange [ FunctionDeclaration "foo" eFuncArgs iI32 eFuncBody 
                                   , ReturnStatement (iLit 45) ]

            Program [ FunctionDeclaration "foo" eFuncArgs iI32 eFuncBody 
                    , ReturnStatement (FunctionCall "foo" [Variable "bar", iLit 32]) ]
                `shouldSatisfy`
                    optimizeChange [ FunctionDeclaration "foo" eFuncArgs iI32 eFuncBody 
                                   , ReturnStatement (BinaryOp Add (Variable "bar") (iLit 32)) ]

            Program [ FunctionDeclaration "foo" eFuncArgsM iI32 eFuncBody 
                    , ReturnStatement (FunctionCall "foo" [Variable "bar", iLit 32]) ]
                `shouldSatisfy`
                    optimizeNoChange

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
iI32 = PrimitiveType Immutable I32

iI32M :: Type
iI32M = PrimitiveType Mutable I32

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
eVar n = EVariable n iI32M (ELiteral $ IntLiteral 0)

eVarI :: String -> Integer -> EnvVar
eVarI n i = EVariable n iI32M (iLit i)

eVarF :: String -> Double -> EnvVar
eVarF n f = EVariable n fF32 (fLit f)

eDefFunc :: String -> EnvVar
eDefFunc n = EFunction n [("foo", iI32)] iI32 [ ReturnStatement (Variable "foo") ]

eFuncBody :: Body
eFuncBody = [ReturnStatement $ BinaryOp Add (Variable "a") (Variable "b")]

eFuncArgs :: [Field]
eFuncArgs = [("a", iI32), ("b", iI32)]

eFuncArgsM :: [Field]
eFuncArgsM = [("a", iI32M), ("b", iI32M)]

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
eFunc n = EFunction n eFuncArgs iI32 eFuncBody

tEnum :: Type
tEnum = EnumType Immutable (EnumT ["RED", "GREEN", "BLUE"])

sType :: String -> Statement
sType n = TypeDeclaration n tEnum

eType :: String -> EnvVar
eType n = EType n tEnum

envWithType :: String -> Env
envWithType n = envWith [eType n]

envWithVar :: String -> Env
envWithVar n = envWith [eVar n]

envWithFunc :: String -> Env
envWithFunc n = envWith [eFunc n]

envWith :: Scope -> Env
envWith s = Env [s]

envWithStuff :: Env
envWithStuff = Env [[eFunc "foo", eType "bar"], [eVar "baz"]]

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
prog = Program [ VariableDeclaration "foo" iI32M Nothing
               , VariableDeclaration "bar" iI32M Nothing
               , VariableDeclaration "baz" iI32M (Just $ iLit 42)
               , VariableReAssignment "bar" (iLit 12)
               , eWhile ]

optimizeChange :: Body -> Program -> Bool
optimizeChange expected p = case optimizeProg p of
    Ok (Program b) -> b == expected
    _ -> False

optimizeNoChange :: Program -> Bool
optimizeNoChange p = case optimizeProg p of
    Ok p' -> p == p'
    _ -> False
