module Interpreter.Data.Environment
    (
        Env(..),
        EnvVar(..),
        Scope,
        env,
        fromEnv,
        pushScope,
        popScope,
        pushVariable,
        pushFunction,
        pushType,
        pushEnv,
        assignVar,
        envAll,
        envAllNames
    )
where

import Parser.Data.Ast
import Utils.Data.Result
import Data.List(intercalate)

data EnvVar = EVariable String Type Expression
            | EFunction String [Field] Type Body
            | EType String Type
            deriving (Eq)

showField :: Field -> String
showField (n, t) = n ++ ": " ++ show t

instance Show EnvVar where
    show (EVariable n t v) = "[ var  ] " ++ n ++ ": " ++ show t ++ " = " ++ show v
    show (EFunction n a t _) =
        "[ func ] " ++ n ++ " (" ++ intercalate ", " (map showField a) ++ ") -> " ++ show t
    show (EType n t) = "[ type ] " ++ n ++ " -> " ++ show t

type Scope = [EnvVar]

newtype Env = Env [Scope]
              deriving (Eq)

instance Show Env where
    show (Env ss) = "\n-- Environment --\n" ++ (unlines . map showScope) ss ++ "---\n"
        where
            showScope = ("-- Scope --\n" ++) . unlines . map (("- " ++) . show)

env :: Env
env = Env []

pushScope :: Env -> Scope -> Env
pushScope (Env ss) s = Env (s:ss)

popScope :: Env -> Env
popScope (Env (_:xs)) = Env xs
popScope e = e

varName :: EnvVar -> String
varName (EVariable n _ _) = n
varName (EFunction n _ _ _) = n
varName (EType n _) = n

fromScope :: Scope -> String -> Maybe EnvVar
fromScope [] _ = Nothing
fromScope (x:xs) n | varName x == n = Just x
                   | otherwise = fromScope xs n

envAll :: Env -> Scope
envAll (Env ss) = concat ss

envAllNames :: Env -> [String]
envAllNames = map varName . envAll

fromEnv :: Env -> String -> Result String EnvVar
fromEnv (Env []) n = Err $ n ++ " is undefined"
fromEnv (Env (x:xs)) n = case fromScope x n of
    Just v -> Ok v
    Nothing -> fromEnv (Env xs) n

pushEnv :: Env -> EnvVar -> Env
pushEnv (Env []) v = pushScope env [v]
pushEnv (Env (x:xs)) v = Env ((v:x):xs)

pushVariable :: Env -> Statement -> Result String Env
pushVariable e (VariableDeclaration n t (Just v)) = case fromEnv e n of
    Ok _ -> Err $ n ++ " redefined"
    _ -> Ok $ pushEnv e (EVariable n t v)
pushVariable _ (VariableDeclaration n _ Nothing) =
    Err $ "Variable " ++ n ++ " must have a value to be added to env"
pushVariable _ v = Err $ "Bad variable type (" ++ show v ++ ")"

assignScopeVar :: Scope -> String -> Expression -> Maybe Scope
assignScopeVar [] _ _ = Nothing
assignScopeVar s n v = assign s []
    where
        assign [] _ = Nothing
        assign ((EVariable vn vt vv):xs) h
            | vn == n = Just (h ++ [EVariable vn vt v] ++ xs)
            | otherwise = assign xs (h ++ [EVariable vn vt vv])
        assign (x:xs) h = assign xs (h ++ [x])

validateAssign :: Env -> String -> Maybe String
validateAssign e n = case fromEnv e n of
    Err m -> Just m
    Ok (EVariable _ t _) | isMutable t -> Nothing
    Ok EVariable {} -> Just $ "Variable " ++ n ++ " is constant"
    Ok _ -> Just $ n ++ " is not assignable"
    where
        isMutable (PrimitiveType Mutable _) = True
        isMutable (PointerType Mutable _) = True
        isMutable (StructType Mutable _) = True
        isMutable (ArrayType Mutable _) = True
        isMutable (EnumType Mutable _) = True
        isMutable (CustomType Mutable _) = True
        isMutable _ = False

assignVar :: Env -> String -> Expression -> Result String Env
assignVar e n v = case validateAssign e n of
    Just s -> Err s
    Nothing -> assign e
        where
            assign (Env []) = Err $ n ++ "is undefined"
            assign (Env (x:xs)) = case assignScopeVar x n v of
                Just x' -> Ok (Env (x':xs))
                Nothing -> case assignVar (Env xs) n v of
                    Ok (Env xs') -> Ok $ Env (x:xs') 
                    Err m -> Err m

pushFunction :: Env -> Statement -> Result String Env
pushFunction e (FunctionDeclaration n a t b) = case fromEnv e n of
    Ok _ -> Err $ n ++ " redefined"
    _ -> Ok $ pushEnv e (EFunction n a t b)
pushFunction _ f = Err $ "Bad function type (" ++ show f ++ ")"

pushType :: Env -> Statement -> Result String Env
pushType e (TypeDeclaration n t) = case fromEnv e n of
    Ok _ -> Err $ n ++ " redefined"
    _ -> Ok $ pushEnv e (EType n t)
pushType _ t = Err $ "Bad type definition (" ++ show t ++ ")"