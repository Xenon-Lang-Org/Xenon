module Interpreter.Data.Environment
    (
        Env(..),
        EnvVar(..),
        env,
        setGlobal,
        setLocal,
        setIsGlobal,
        setLocalScope,
        setGlobalScope,
        fromLocal,
        fromGlobal,
        fromEnv,
        pushVariable,
        pushFunction,
        pushType,
        assignVar
    )
where

import Parser.Data.Ast
import Utils.Data.Result

data EnvVar = EVariable String Type Expression
            | EFunction String [Field] Type Body
            | EType String TypeDefinition
            deriving (Show)

type Scope = [EnvVar]

data Env = Env { global     :: Scope
               , local      :: Scope
               , isGlobal   :: Bool}

instance Show Env where
    show (Env g l ig) = 
        "\n-- " ++ getName ig ++ " Environment --\n" ++
        "\n[Global]\n" ++ unlines (map (("- " ++) . show) g) ++
        "\n[Local]\n" ++ unlines (map (("- " ++) . show) l) ++
        "---\n"
        where
            getName True = "Global"
            getName False = "Local"

env :: Bool -> Env
env = Env [] []

setGlobal :: Env -> Env
setGlobal (Env g l _) = Env g l True

setLocal :: Env -> Env
setLocal (Env g l _) = Env g l False

setIsGlobal :: Bool -> Env -> Env
setIsGlobal True = setGlobal
setIsGlobal False = setLocal

setGlobalScope :: Env -> Scope -> Env
setGlobalScope (Env _ l ig) g = Env g l ig

setLocalScope :: Env -> Scope -> Env
setLocalScope (Env g _ ig) l = Env g l ig

fromLocal :: Bool -> Scope -> Env
fromLocal = setLocalScope . env

fromGlobal :: Bool -> Scope -> Env
fromGlobal = setGlobalScope . env

varName :: EnvVar -> String
varName (EVariable name _ _) = name
varName (EFunction name _ _ _) = name
varName (EType name _) = name

assignScopeVar :: Scope -> String -> Expression -> Result String Scope
assignScopeVar [] n _ = Err $ n ++ " is undefined"
assignScopeVar s n v = assign s []
    where
        assign [] _ = Err $ n ++ " is undefined"
        assign ((EVariable vn vt vv):xs) h
            | vn == n = Ok (h ++ [EVariable vn vt v] ++ xs)
            | otherwise = assign xs (h ++ [EVariable vn vt vv])
        assign (x:xs) h = assign xs (h ++ [x])

fromScope :: Scope -> String -> Result String EnvVar
fromScope [] name = Err $ name ++ " is undefined" 
fromScope (x:xs) name | varName x == name = Ok x
                      | otherwise = fromScope xs name

fromEnv :: Env -> String -> Result String EnvVar
fromEnv (Env g l _) name = case fromScope l name of
    Ok s -> Ok s
    _ -> fromScope g name

pushEnv :: Env -> EnvVar -> Env
pushEnv (Env g l True) v = Env (v:g) l True
pushEnv (Env g l False) v = Env g (v:l) False

pushVariable :: Env -> Statement -> Result String Env
pushVariable e (VariableDeclaration n t (Just v)) = case fromEnv e n of 
    Ok _ -> Err $ n ++ " redefined"
    _ -> Ok $ pushEnv e (EVariable n t v)
pushVariable _ (VariableDeclaration n _ Nothing) =
    Err $ "Variable " ++ n ++ " must have a value to be added to env"
pushVariable _ v = Err $ "Bad variable type (" ++ show v ++ ")"

assignVar :: Env -> String -> Expression -> Result String Env
assignVar e n v = case assignScopeVar (local e) n v of
    Ok s -> Ok $ setLocalScope e s
    _ -> case assignScopeVar (global e) n v of
        Ok s -> Ok $ setGlobalScope e s
        Err msg -> Err msg

pushFunction :: Env -> Statement -> Result String Env
pushFunction e (FunctionDeclaration n a t b) = case fromEnv e n of
    Ok _ -> Err $ n ++ " redefined"
    _ -> Ok $ pushEnv e (EFunction n a t b)
pushFunction _ f = Err $ "Bad function type (" ++ show f ++ ")"

typeDefName :: TypeDefinition -> String
typeDefName (StructDeclaration n _) = n
typeDefName (ArrayDeclaration n _) = n
typeDefName (EnumDeclaration n _) = n

pushType :: Env -> Statement -> Result String Env
pushType e (TypeDeclaration t) = case fromEnv e n of
    Ok _ -> Err $ n ++ " redefined"
    _ -> Ok $ pushEnv e (EType n t)
    where
        n = typeDefName t
pushType _ t = Err $ "Bad type definition (" ++ show t ++ ")"
