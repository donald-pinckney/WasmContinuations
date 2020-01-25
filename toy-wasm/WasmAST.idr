module WasmAST


public export
data WasmType = WasmTypeI64 | WasmTypeF64

public export
data WasmValue = WasmValueI64 Int | WasmValueF64 Double

export
wasmTypeOfWasmValue : WasmValue -> WasmType
wasmTypeOfWasmValue (WasmValueI64 x) = WasmTypeI64
wasmTypeOfWasmValue (WasmValueF64 x) = WasmTypeF64

public export
idrisTypeOfWasmType : WasmType -> Type
idrisTypeOfWasmType WasmTypeI64 = Int
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
                | WasmInstrI64And
                | WasmInstrI64Or
                | WasmInstrI64Eqz
                | WasmInstrF64Add
                | WasmInstrF64Sub
                | WasmInstrF64Mul
                | WasmInstrF64Div
                | WasmInstrF64Eqz
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
    -- TODO: linear memory


export
implementation Eq WasmType where
    WasmTypeF64 == WasmTypeI64 = False
    WasmTypeF64 == WasmTypeF64 = True
    WasmTypeI64 == WasmTypeI64 = True
    WasmTypeI64 == WasmTypeF64 = False

export
implementation Show WasmType where
    show WasmTypeI64 = "i64"
    show WasmTypeF64 = "f64"

export
implementation Show WasmValue where
    show (WasmValueI64 x) = show x
    show (WasmValueF64 x) = show x
