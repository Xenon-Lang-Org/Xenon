module Interpreter.Data.Environment
    (
        Env(..),
        EnvVar(..),
        newEnv,
        isGlobal,
        setGlobal,
        setLocal,
        fromEnv,
        pushVariable,
        pushFunction,
        pushType,
    )
where

import Parser.Data.Ast
import Utils.Data.Result

data EnvVar = EVariable String Type Expression
            | EFunction String [Field] Type Body
            | EType String TypeDefinition
            deriving (Show)

data Env = Env { global     :: [EnvVar]
               , local      :: [EnvVar]
               , isGlobal   :: Bool
               , returnVal  :: Expression}

instance Show Env where
    show (Env g l ig r) = 
        "\n-- " ++ getName ig ++ " Environment --\n" ++ unlines (map show g) ++
        "\n[Global]\n" ++ unlines (map (("- " ++) . show) g) ++
        "\n[Local]\n" ++ unlines (map (("- " ++) . show) l) ++
        "\n[ReturnValue]: " ++ show r
        where
            getName True = "Global"
            getName False = "Local"

newEnv :: Bool -> Env
newEnv ig = Env [] [] ig (ELiteral $ IntLiteral 0)

setGlobal :: Env -> Env
setGlobal (Env g l _ r) = Env g l True r

setLocal :: Env -> Env
setLocal (Env g l _ r) = Env g l False r

varName :: EnvVar -> String
varName (EVariable name _ _) = name
varName (EFunction name _ _ _) = name
varName (EType name _) = name

fromScope :: [EnvVar] -> String -> Result String EnvVar
fromScope [] name = Err $ name ++ " is undefined" 
fromScope (x:xs) name | varName x == name = Ok x
                      | otherwise = fromScope xs name

fromEnv :: Env -> String -> Result String EnvVar
fromEnv (Env g l _ _) name = case fromScope l name of
    Ok s -> Ok s
    _ -> fromScope g name

pushEnv :: Env -> EnvVar -> Env
pushEnv (Env g l True r) v = Env (v:g) l True r
pushEnv (Env g l False r) v = Env g (v:l) False r

pushVariable :: Env -> Statement -> Result String Env
pushVariable env (VariableDeclaration n t (Just v)) = case fromEnv env n of 
    Ok _ -> Err $ n ++ " redefined"
    _ -> Ok $ pushEnv env (EVariable n t v)
pushVariable _ (VariableDeclaration n _ Nothing) =
    Err $ "Variable " ++ n ++ " must have a value to be added to env"
pushVariable _ v = Err $ "Bad variable type (" ++ show v ++ ")"

pushFunction :: Env -> Statement -> Result String Env
pushFunction env (FunctionDeclaration n a t b) = case fromEnv env n of
    Ok _ -> Err $ n ++ " redefined"
    _ -> Ok $ pushEnv env (EFunction n a t b)
pushFunction _ f = Err $ "Bad function type (" ++ show f ++ ")"

typeDefName :: TypeDefinition -> String
typeDefName (StructDeclaration n _) = n
typeDefName (ArrayDeclaration n _) = n
typeDefName (EnumDeclaration n _) = n

pushType :: Env -> Statement -> Result String Env
pushType env (TypeDeclaration t) = case fromEnv env n of
    Ok _ -> Err $ n ++ " redefined"
    _ -> Ok $ pushEnv env (EType n t)
    where
        n = typeDefName t
pushType _ t = Err $ "Bad type definition (" ++ show t ++ ")"
