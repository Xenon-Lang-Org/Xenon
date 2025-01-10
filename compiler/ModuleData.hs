module ModuleData (
    WASMModule(..),
    TypeSection(..),
    FunctionType(..),
    ImportSection(..),
    Import(..),
    ExportSection(..),
    Export(..),
    ExportKind(..),
    CodeSection(..),
    Function(..),
    Instruction(..),
    ValueType(..),
    Mutability(..)
) where

data WASMModule = WASMModule {
    types     :: [TypeSection],
    imports   :: [ImportSection],
    functions :: [Function],
    exports   :: [ExportSection],
    codes     :: [CodeSection],
    memories  :: [(Int, Int)],
    globals   :: [(ValueType, Mutability, [Instruction])]
} deriving (Show, Eq)

data TypeSection = TypeSection FunctionType
    deriving (Show, Eq)

data FunctionType = FunctionType {
    funcparams  :: [ValueType],
    results     :: [ValueType]
} deriving (Show, Eq)

data ImportSection = ImportSection Import
    deriving (Show, Eq)

data Import = Import {
    moduleName :: String,
    fieldName  :: String,
    importType :: ValueType
} deriving (Show, Eq)

data ExportSection = ExportSection Export
    deriving (Show, Eq)

data Export = Export {
    exportName :: String,
    exportKind :: ExportKind
} deriving (Show, Eq)

data ExportKind
    = FunctionExport
    | TableExport
    | MemoryExport
    | GlobalExport
    deriving (Show, Eq)

data CodeSection = CodeSection {
    locals   :: [ValueType],
    codebody :: [Instruction]
} deriving (Show, Eq)

data Function = Function {
    typeIndex    :: Int,
    functionCode :: CodeSection
} deriving (Show, Eq)

data Instruction
    = ModuleConstI32 Int
    | ModuleConstI64 Int
    | ModuleConstF32 Float
    | ModuleConstF64 Double

    | ModuleAdd ValueType
    | ModuleSub ValueType
    | ModuleMul ValueType
    | ModuleDiv ValueType
    | ModuleMod ValueType

    | ModuleBitAnd ValueType
    | ModuleBitOr ValueType
    | ModuleBitXor ValueType
    | ModuleBitNot ValueType
    | ModuleShl ValueType
    | ModuleShr ValueType


    | ModuleLocalGet Int
    | ModuleLocalSet Int

    | ModuleGlobalGet Int
    | ModuleGlobalSet Int

    | ModuleCall Int

    | ModuleGt ValueType
    | ModuleLt ValueType
    | ModuleEq ValueType
    | ModuleNeq ValueType
    | ModuleLe ValueType
    | ModuleGe ValueType
    | ModuleIf (Maybe ValueType) [Instruction] [Instruction]

    | ModuleEqz ValueType

    | ModuleReturn

    | ModuleBlock (Maybe ValueType) [Instruction]
    | ModuleLoop (Maybe ValueType) [Instruction]
    | ModuleBrIf Int
    | ModuleBr Int

    | ModuleEnd
    deriving (Show, Eq)

data ValueType
    = ModuleI32
    | ModuleI64
    | ModuleF32
    | ModuleF64
    deriving (Show, Eq)

data Mutability = Mutable | Immutable
    deriving (Show, Eq)
