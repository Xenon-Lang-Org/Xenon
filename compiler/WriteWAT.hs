module WriteWAT (printModule) where

import ModuleData

printModule :: WASMModule -> IO ()
printModule wasmModule = do
    putStrLn "(module"
    putStrLn "  ;; Types ;;"
    mapM_ showTypeSection (types wasmModule)
    putStrLn "\n  ;; Functions ;;"
    mapM_ showFunction (functions wasmModule)
    putStrLn "\n  ;; Exports ;;"
    mapM_ showExportSection (exports wasmModule)
    putStrLn "\n  ;; Globals ;;"
    mapM_ showGlobal (globals wasmModule)
    putStrLn ")"

-- Show a type section
showTypeSection :: TypeSection -> IO ()
showTypeSection (TypeSection (FunctionType params printResults)) = do
    putStrLn $ "  (type (func (param " ++ unwords (map showValueType params) ++ ") (result " ++ unwords (map showValueType printResults) ++ ")))"

-- Show a function
showFunction :: Function -> IO ()
showFunction (Function typeIdx (CodeSection printLocals printCodebody)) = do
    putStrLn $ "  (func (; " ++ show typeIdx ++ " ;) (param " ++ unwords (map showValueType printLocals) ++ ")"
    mapM_ showInstruction printCodebody
    putStrLn "  )"

-- Show an export section
showExportSection :: ExportSection -> IO ()
showExportSection (ExportSection (Export name kind)) = do
    let kindStr = case kind of
            FunctionExport -> "func"
            TableExport    -> "table"
            MemoryExport   -> "memory"
            GlobalExport   -> "global"
    putStrLn $ "  (export \"" ++ name ++ "\" (" ++ kindStr ++ " 0))"

-- Show globals
showGlobal :: (ValueType, Mutability, [Instruction]) -> IO ()
showGlobal (valType, mut, instrs) = do
    let mutStr = case mut of
            ModuleMutable   -> "mut "
            ModuleImmutable -> "const "
    putStrLn $ "  (global (" ++ mutStr ++ showValueType valType ++ ")"
    mapM_ showInstruction instrs
    putStrLn "  )"

-- Show an instruction
showInstruction :: Instruction -> IO ()
showInstruction (ModuleLoop _ instrs) = do
    putStrLn "    (loop"
    mapM_ showInstruction instrs
    putStrLn "    )"
showInstruction (ModuleBlock _ instrs) = do
    putStrLn "    (block"
    mapM_ showInstruction instrs
    putStrLn "    )"
showInstruction (ModuleBr idx) = putStrLn $ "    br " ++ show idx
showInstruction (ModuleBrIf idx) = putStrLn $ "    br_if " ++ show idx
showInstruction ModuleReturn = putStrLn "    return"
showInstruction (ModuleLocalGet idx) = putStrLn $ "    local.get " ++ show idx
showInstruction (ModuleLocalSet idx) = putStrLn $ "    local.set " ++ show idx
showInstruction (ModuleGlobalGet idx) = putStrLn $ "    global.get " ++ show idx
showInstruction (ModuleGlobalSet idx) = putStrLn $ "    global.set " ++ show idx
showInstruction (ModuleConstI32 n) = putStrLn $ "    i32.const " ++ show n
showInstruction (ModuleConstI64 n) = putStrLn $ "    i64.const " ++ show n
showInstruction (ModuleConstF32 f) = putStrLn $ "    f32.const " ++ show f
showInstruction (ModuleConstF64 d) = putStrLn $ "    f64.const " ++ show d
showInstruction (ModuleAdd vt) = putStrLn $ "    " ++ showValueType vt ++ ".add"
showInstruction (ModuleSub vt) = putStrLn $ "    " ++ showValueType vt ++ ".sub"
showInstruction (ModuleMul vt) = putStrLn $ "    " ++ showValueType vt ++ ".mul"
showInstruction (ModuleDiv vt) = putStrLn $ "    " ++ showValueType vt ++ ".div_s"
showInstruction (ModuleMod vt) = putStrLn $ "    " ++ showValueType vt ++ ".rem_s"
showInstruction (ModuleGt vt) = putStrLn $ "    " ++ showValueType vt ++ ".gt_s"
showInstruction (ModuleLt vt) = putStrLn $ "    " ++ showValueType vt ++ ".lt_s"
showInstruction (ModuleEq vt) = putStrLn $ "    " ++ showValueType vt ++ ".eq"
showInstruction (ModuleNeq vt) = putStrLn $ "    " ++ showValueType vt ++ ".ne"
showInstruction (ModuleLe vt) = putStrLn $ "    " ++ showValueType vt ++ ".le_s"
showInstruction (ModuleGe vt) = putStrLn $ "    " ++ showValueType vt ++ ".ge_s"
showInstruction (ModuleEqz vt) = putStrLn $ "    " ++ showValueType vt ++ ".eqz"
showInstruction (ModuleBitAnd vt) = putStrLn $ "    " ++ showValueType vt ++ ".and"
showInstruction (ModuleBitOr vt) = putStrLn $ "    " ++ showValueType vt ++ ".or"
showInstruction (ModuleBitXor vt) = putStrLn $ "    " ++ showValueType vt ++ ".xor"
showInstruction (ModuleBitNot vt) = putStrLn $ "    " ++ showValueType vt ++ ".not"
showInstruction (ModuleShl vt) = putStrLn $ "    " ++ showValueType vt ++ ".shl"
showInstruction (ModuleShr vt) = putStrLn $ "    " ++ showValueType vt ++ ".shr_s"
showInstruction (ModuleAnd vt) = putStrLn $ "    " ++ showValueType vt ++ ".and"
showInstruction (ModuleOr vt) = putStrLn $ "    " ++ showValueType vt ++ ".or"
showInstruction (ModuleIf _ thenInstrs elseInstrs) = do
    putStrLn "    (if"
    putStrLn "      (then"
    mapM_ showInstruction thenInstrs
    putStrLn "      )"
    putStrLn "      (else"
    mapM_ showInstruction elseInstrs
    putStrLn "      )"
    putStrLn "    )"
showInstruction (ModuleCall idx) = putStrLn $ "    call " ++ show idx
showInstruction ModuleEnd = putStrLn "    end"


-- Show value types
showValueType :: ValueType -> String
showValueType ModuleI32 = "i32"
showValueType ModuleI64 = "i64"
showValueType ModuleF32 = "f32"
showValueType ModuleF64 = "f64"
