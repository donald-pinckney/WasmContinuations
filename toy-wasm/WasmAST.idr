module WasmAST

%default covering

public export
data WasmType = WasmTypeI64 | WasmTypeF64 | WasmTypeI32

public export
data WasmValue = WasmValueI64 Int | WasmValueF64 Double | WasmValueI32 Int

export
wasmTypeOfWasmValue : WasmValue -> WasmType
wasmTypeOfWasmValue (WasmValueI64 x) = WasmTypeI64
wasmTypeOfWasmValue (WasmValueI32 x) = WasmTypeI32
wasmTypeOfWasmValue (WasmValueF64 x) = WasmTypeF64

public export
idrisTypeOfWasmType : WasmType -> Type
idrisTypeOfWasmType WasmTypeI64 = Int
idrisTypeOfWasmType WasmTypeI32 = Int
idrisTypeOfWasmType WasmTypeF64 = Double

public export
data WasmInstr =  WasmInstrConst WasmValue
                | WasmInstrDrop
                | WasmInstrLocalGet Int
                | WasmInstrLocalSet Int
                | WasmInstrBlock (Maybe WasmType) (List WasmInstr)
                | WasmInstrIf (Maybe WasmType) (List WasmInstr) (List WasmInstr)
                | WasmInstrLoop (Maybe WasmType) (List WasmInstr)
                | WasmInstrBr Int
                | WasmInstrBrIf Int
                | WasmInstrCall Int
                | WasmInstrI64Add
                | WasmInstrI64Sub
                | WasmInstrI64Mul
                | WasmInstrI64Div_s
                | WasmInstrI64Rem_s
                | WasmInstrI32And
                | WasmInstrI32Or
                | WasmInstrI32Eqz
                | WasmInstrI32Eq
                | WasmInstrF64Add
                | WasmInstrF64Sub
                | WasmInstrF64Mul
                | WasmInstrF64Div
                | WasmInstrI64Eq
                | WasmInstrI64Lt_s
                | WasmInstrI64Gt_s
                | WasmInstrI64Le_s
                | WasmInstrI64Ge_s
                | WasmInstrF64Eq
                | WasmInstrF64Lt
                | WasmInstrF64Gt
                | WasmInstrF64Le
                | WasmInstrF64Ge
                | WasmInstrWrapI64ToI32

public export
record WasmFunction where
    constructor MkWasmFunction
    paramTypes : List WasmType
    resultType : WasmType
    localTypes : List WasmType
    body : List WasmInstr
    id : Int

public export
record WasmModule where
    constructor MkWasmModule
    funcs : List WasmFunction
    start : Int
    start_type : WasmType
    -- TODO: linear memory


export
implementation Eq WasmType where
    WasmTypeF64 == WasmTypeF64 = True
    WasmTypeI64 == WasmTypeI64 = True
    WasmTypeI32 == WasmTypeI32 = True
    _ == _ = False

export
implementation Eq WasmValue where
    (WasmValueI64 x) == (WasmValueI64 y) = x == y
    (WasmValueF64 x) == (WasmValueF64 y) = x == y
    (WasmValueI32 x) == (WasmValueI32 y) = x == y
    _ == _ = False

export
implementation Show WasmType where
    show WasmTypeI64 = "i64"
    show WasmTypeI32 = "i32"
    show WasmTypeF64 = "f64"

export
implementation Show WasmValue where
    show (WasmValueI64 x) = show x
    show (WasmValueI32 x) = show x
    show (WasmValueF64 x) = show x

export
implementation Show WasmFunction where
    show s = "[func]"
