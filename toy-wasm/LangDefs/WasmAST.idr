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
data WasmInstr' v local func label t =  WasmInstrConst v
                | WasmInstrDrop
                | WasmInstrLocalGet local
                | WasmInstrLocalSet local
                | WasmInstrLocalTee local
                | WasmInstrBlock (Maybe t) (List (WasmInstr' v local func label t))
                | WasmInstrIf (Maybe t) (List (WasmInstr' v local func label t)) (List (WasmInstr' v local func label t))
                | WasmInstrLoop (Maybe t) (List (WasmInstr' v local func label t))
                | WasmInstrBr label
                | WasmInstrBrIf label
                | WasmInstrCall func
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
                | WasmInstrI64Shr_u
                | WasmInstrI64Shl
                | WasmInstrI64And
                | WasmInstrI64Neq
                | WasmInstrI64Eqz
                | WasmInstrF64Neg

                | WasmInstrI64ExtendI32_s
                | WasmInstrI64TruncF64_s
                | WasmInstrF64ConvertI32_s
                | WasmInstrF64ConvertI64_s
                | WasmInstrF64Neq


public export
WasmInstr : Type
WasmInstr = WasmInstr' WasmValue Int Int Int WasmType

public export
record WasmFunction where
    constructor MkWasmFunction
    paramTypes : List WasmType
    resultType : Maybe WasmType
    localTypes : List WasmType
    body : List WasmInstr
    id : Int

public export
record WasmModule where
    constructor MkWasmModule
    funcs : List WasmFunction
    start : Int
    start_type : Maybe WasmType
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



export
implementation Eq WasmInstr where
    (WasmInstrConst x) == (WasmInstrConst y) = x == y
    (WasmInstrDrop) == (WasmInstrDrop) = True
    (WasmInstrLocalGet x) == (WasmInstrLocalGet y) = x == y
    (WasmInstrLocalSet x) == (WasmInstrLocalSet y) = x == y
    (WasmInstrLocalTee x) == (WasmInstrLocalTee y) = x == y
    (WasmInstrBlock t1 x) == (WasmInstrBlock t2 y) = t1 == t2 && assert_total (x == y)
    (WasmInstrIf t1 x z) == (WasmInstrIf t2 y w) = t1 == t2 && assert_total (x == y) && assert_total (z == w)
    (WasmInstrLoop t1 x) == (WasmInstrLoop t2 y) = t1 == t2 && assert_total (x == y)
    (WasmInstrBr x) == (WasmInstrBr y) = x == y
    (WasmInstrBrIf x) == (WasmInstrBrIf y) = x == y
    (WasmInstrCall x) == (WasmInstrCall y) = x == y
    WasmInstrI64Add == WasmInstrI64Add = True
    WasmInstrI64Sub == WasmInstrI64Sub = True
    WasmInstrI64Mul == WasmInstrI64Mul = True
    WasmInstrI64Div_s == WasmInstrI64Div_s = True
    WasmInstrI64Rem_s == WasmInstrI64Rem_s = True
    WasmInstrI32And == WasmInstrI32And = True
    WasmInstrI32Or == WasmInstrI32Or = True
    WasmInstrI32Eqz == WasmInstrI32Eqz = True
    WasmInstrI32Eq == WasmInstrI32Eq = True
    WasmInstrF64Add == WasmInstrF64Add = True
    WasmInstrF64Sub == WasmInstrF64Sub = True
    WasmInstrF64Mul == WasmInstrF64Mul = True
    WasmInstrF64Div == WasmInstrF64Div = True
    WasmInstrI64Eq == WasmInstrI64Eq = True
    WasmInstrI64Lt_s == WasmInstrI64Lt_s = True
    WasmInstrI64Gt_s == WasmInstrI64Gt_s = True
    WasmInstrI64Le_s == WasmInstrI64Le_s = True
    WasmInstrI64Ge_s == WasmInstrI64Ge_s = True
    WasmInstrF64Eq == WasmInstrF64Eq = True
    WasmInstrF64Lt == WasmInstrF64Lt = True
    WasmInstrF64Gt == WasmInstrF64Gt = True
    WasmInstrF64Le == WasmInstrF64Le = True
    WasmInstrF64Ge == WasmInstrF64Ge = True
    WasmInstrWrapI64ToI32 == WasmInstrWrapI64ToI32 = True
    WasmInstrI64Shr_u == WasmInstrI64Shr_u = True
    WasmInstrI64Shl == WasmInstrI64Shl = True
    WasmInstrI64And == WasmInstrI64And = True
    WasmInstrI64Neq == WasmInstrI64Neq = True
    WasmInstrI64Eqz == WasmInstrI64Eqz = True
    WasmInstrF64Neg == WasmInstrF64Neg = True

    WasmInstrI64ExtendI32_s == WasmInstrI64ExtendI32_s = True
    WasmInstrI64TruncF64_s == WasmInstrI64TruncF64_s = True
    WasmInstrF64ConvertI32_s == WasmInstrF64ConvertI32_s = True
    WasmInstrF64ConvertI64_s == WasmInstrF64ConvertI64_s = True
    WasmInstrF64Neq == WasmInstrF64Neq = True

    _ == _ = False
