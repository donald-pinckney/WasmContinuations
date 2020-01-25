module WasmInterp

import WasmAST

public export
State : Type
State = List WasmValue

public export
alloc_types : List WasmType -> State
alloc_types [] = []
alloc_types (WasmTypeI64 :: xs) = WasmValueI64 0 :: (alloc_types xs)
alloc_types (WasmTypeF64 :: xs) = WasmValueF64 0.0 :: (alloc_types xs)

public export
assert_type : (t : WasmType) -> WasmValue -> Either String (idrisTypeOfWasmType t)
assert_type WasmTypeI64 (WasmValueI64 x) = Right x
assert_type WasmTypeF64 (WasmValueF64 x) = Right x
assert_type t@WasmTypeI64 x@(WasmValueF64 x') = Left $ "Expected " ++ show t ++ " got " ++ show (wasmTypeOfWasmValue x) ++ " (" ++ show x ++ ")"
assert_type t@WasmTypeF64 x@(WasmValueI64 x') = Left $ "Expected " ++ show t ++ " got " ++ show (wasmTypeOfWasmValue x) ++ " (" ++ show x ++ ")"

public export
lookup_list : Int -> List x -> Either String x
lookup_list i [] = Left "Index out of bounds"
lookup_list i (y :: xs) = if i == 0 then Right y else lookup_list (i-1) xs

mutual
    public export
    interp_instr : (mod : WasmModule) -> (frame : State) -> (stack : List WasmValue) -> (instr : WasmInstr) -> Either String (State, List WasmValue)
    interp_instr mod frame stack instr = ?interp_instr_rhs

    public export
    interp_instrs : (mod : WasmModule) -> (frame : State) -> (stack : List WasmValue) -> (instrs : List WasmInstr) -> Either String WasmValue
    interp_instrs mod frame [] [] = Left "Expected value to be on stack"
    interp_instrs mod frame (x :: []) [] = Right x
    interp_instrs mod frame (x :: (y :: xs)) [] = Left "Expected ONE value to be on stack"
    interp_instrs mod frame stack (i :: is) = do
        (frame', stack') <- interp_instr mod frame stack i
        interp_instrs mod frame' stack' is

    public export
    interp_call : WasmModule -> WasmFunction -> List WasmValue -> Either String WasmValue
    interp_call mod (MkWasmFunction paramTypes resultType localTypes body id) args =
        let frame = args ++ alloc_types localTypes in
        interp_instrs mod frame [] body

public export
interp_module : WasmModule -> Either String WasmValue
interp_module mod@(MkWasmModule funcs start) = do
    start_f <- lookup_list start funcs
    if length (paramTypes start_f) /= 0
        then Left "Start function should take no arguments"
        else interp_call mod start_f []
