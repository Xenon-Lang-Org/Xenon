module InterpreterSpec (main, spec) where

import Test.Hspec
import Interpreter.Data.Environment
import Interpreter.System.BinaryOperation
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
            pushVariable (env True) (VariableDeclaration "foo" iType (Just $ iLit 0)) 
                `shouldBe` Ok (envWithVar "foo")
            pushFunction (env True) (FunctionDeclaration "bar" eFuncArgs iType eFuncBody)
                `shouldBe` Ok (envWithFunc "bar")
            pushType (env True) (sType "baz")
                `shouldBe` Ok (envWithType "baz")
        
        it "should change the value of a variable" $ do
            assignVar (envWithVar "foo") "foo" (iLit 12) `shouldSatisfy` varHasValue "foo" (iLit 12)
            assignVar (envWithVar "foo") "bar" (iLit 12) `shouldSatisfy` isErr 

-- Helpers

iLit :: Integer -> Expression
iLit i = ELiteral $ IntLiteral i

iType :: Type
iType = PrimitiveType I32

eVar :: String -> EnvVar
eVar n = EVariable n iType (ELiteral $ IntLiteral 0)

eFuncBody :: Body
eFuncBody = [ReturnStatement $ BinaryOp Add (Variable "a") (Variable "b")]

eFuncArgs :: [Field]
eFuncArgs = [("a", iType), ("b", iType)]

eFunc :: String -> EnvVar
eFunc n = EFunction n  eFuncArgs iType eFuncBody

tEnum :: String -> TypeDefinition
tEnum n = EnumDeclaration n ["RED", "GREEN", "BLUE"]

sType :: String -> Statement
sType n = TypeDeclaration $ tEnum n

eType :: String -> EnvVar
eType n = EType n (tEnum n)

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