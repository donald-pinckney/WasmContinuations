module WasmInterp

import WasmAST

%default covering

public export
State : Type
State = List WasmValue

public export
alloc_types : List WasmType -> State
alloc_types [] = []
alloc_types (WasmTypeI64 :: xs) = WasmValueI64 0 :: (alloc_types xs)
alloc_types (WasmTypeI32 :: xs) = WasmValueI32 0 :: (alloc_types xs)
alloc_types (WasmTypeF64 :: xs) = WasmValueF64 0.0 :: (alloc_types xs)

public export
assert_type : (t : WasmType) -> WasmValue -> Either String (idrisTypeOfWasmType t)
assert_type WasmTypeI64 (WasmValueI64 x) = Right x
assert_type WasmTypeI32 (WasmValueI32 x) = Right x
assert_type WasmTypeF64 (WasmValueF64 x) = Right x
assert_type t x = Left $ "Expected " ++ show t ++ " got " ++ show (wasmTypeOfWasmValue x) ++ " (" ++ show x ++ ")"

public export
lookup_list : Int -> List x -> Either String x
lookup_list i [] = Left "Index out of bounds"
lookup_list i (y :: xs) = if i == 0 then Right y else lookup_list (i-1) xs

public export
update_list : Int -> x -> List x -> Either String (List x)
update_list i y [] = Left "Index out of bounds"
update_list i y (z :: xs) = if i == 0 then Right (y :: xs) else update_list (i-1) y xs

public export
pop : List WasmValue -> Either String (WasmValue, List WasmValue)
pop [] = Left "expected a non-empty stack"
pop (x :: xs) = Right (x, xs)

public export
pop_n : List a -> Int -> Either String (List a, List a) -- (the poped ones, the remaining ones)
pop_n [] x = if x == 0 then Right ([], []) else Left "could not pop enough things"
pop_n (y :: xs) n = if n == 0
    then Right ([], y :: xs)
    else do
        (popped, rem) <- pop_n xs (n-1)
        pure (y :: popped, rem)


public export
record Label where
    constructor MkLabel
    arity : Int
    stackDepth : Int
    continuation : List WasmInstr

public export
length : Maybe x -> Int
length Nothing = 0
length (Just x) = 1

public export
length_list : List x -> Int
length_list [] = 0
length_list (y :: xs) = 1 + length_list xs

public export
idris_val_to_wasm_value : idrisTypeOfWasmType t -> WasmValue
idris_val_to_wasm_value {t = WasmTypeI64} x = WasmValueI64 x
idris_val_to_wasm_value {t = WasmTypeI32} x = WasmValueI32 x
idris_val_to_wasm_value {t = WasmTypeF64} x = WasmValueF64 x

public export
interp_binop : (at : WasmType) -> (rt : WasmType) ->
                ((idrisTypeOfWasmType at) -> (idrisTypeOfWasmType at) -> (idrisTypeOfWasmType rt)) ->
                List WasmValue -> Either String (List WasmValue)
interp_binop at rt f stack = do
    (y_v, stack') <- pop stack
    y_n <- assert_type at y_v
    (x_v, stack'') <- pop stack'
    x_n <- assert_type at x_v
    let r_n = f x_n y_n
    pure $ idris_val_to_wasm_value {t=rt} r_n :: stack''

public export
interp_unop : (at : WasmType) -> (rt : WasmType) ->
                ((idrisTypeOfWasmType at) -> (idrisTypeOfWasmType rt)) ->
                List WasmValue -> Either String (List WasmValue)
interp_unop at rt f stack = do
    (x_v, stack') <- pop stack
    x_n <- assert_type at x_v
    let r_n = f x_n
    pure $ idris_val_to_wasm_value {t=rt} r_n :: stack'

public export
bool_to_int : Bool -> Int
bool_to_int False = 0
bool_to_int True = 1

public export
int_to_bool : Int -> Bool
int_to_bool x = not (x == 0)

public export
int_and : Int -> Int -> Int
int_and x y = bool_to_int (int_to_bool x && int_to_bool y)

public export
int_or : Int -> Int -> Int
int_or x y = bool_to_int (int_to_bool x || int_to_bool y)

public export
int_not : Int -> Int
int_not x = bool_to_int (not (int_to_bool x))

public export
comp2 : (c -> d) -> (a -> b -> c) -> (a -> b -> d)
comp2 f g x y = f (g x y)

mutual
    public export
    interp_instr : (mod : WasmModule) -> (frame : State) -> (stack : List WasmValue) -> (labels : List Label) -> (after : List WasmInstr) -> (instr : WasmInstr) -> Either String (State, List WasmValue)
    interp_instr mod frame stack labels after (WasmInstrConst x) = Right (frame, x :: stack)
    interp_instr mod frame stack labels after WasmInstrDrop = do
        (v, stack') <- pop stack
        pure (frame, stack')
    interp_instr mod frame stack labels after (WasmInstrLocalGet x) = Right (frame, !(lookup_list x frame) :: stack)
    interp_instr mod frame stack labels after (WasmInstrLocalSet x) = do
        (v, stack') <- pop stack
        Right (!(update_list x v frame), stack')
    interp_instr mod frame stack labels after (WasmInstrBlock t instrs) = do
        let labels' = (MkLabel (length t) (length_list stack) after) :: labels
        interp_instrs mod frame stack labels' instrs
    interp_instr mod frame stack labels after (WasmInstrIf t tr_instrs fa_instrs) = do
        (cw, stack') <- pop stack
        c <- assert_type WasmTypeI32 cw
        let labels' = (MkLabel (length t) (length_list stack') after) :: labels
        if c /= 0
            then interp_instrs mod frame stack' labels' tr_instrs
            else interp_instrs mod frame stack' labels' fa_instrs

    interp_instr mod frame stack labels after (WasmInstrLoop t instrs) = do
        let labels' = (MkLabel 0 (length_list stack) (WasmInstrLoop t instrs :: after)) :: labels
        interp_instrs mod frame stack labels' instrs
    interp_instr mod frame stack labels after (WasmInstrBr li) = do
        l <- lookup_list li labels
        (discarded_labels, labels') <- pop_n labels (li + 1)
        (vals, stack') <- pop_n stack (arity l)
        (discarded_vals, stack'') <- pop_n stack' (length_list stack' - stackDepth l)
        let stack''' = vals ++ stack''
        interp_instrs mod frame stack''' labels' (continuation l)
    interp_instr mod frame stack labels after (WasmInstrBrIf li) = do
        (cw, stack') <- pop stack
        c <- assert_type WasmTypeI32 cw
        if c /= 0
            then interp_instr mod frame stack' labels after (WasmInstrBr li)
            else pure (frame, stack')
    interp_instr mod frame stack labels after (WasmInstrCall fi) = do
        f <- lookup_list fi (funcs mod)
        (args, stack') <- pop_n stack (length_list (paramTypes f))
        v <- interp_call mod f args
        pure (frame, v :: stack)
    interp_instr mod frame stack labels after WasmInstrI64Add = Right (frame, !(interp_binop WasmTypeI64 WasmTypeI64 (+) stack))
    interp_instr mod frame stack labels after WasmInstrI64Sub = Right (frame, !(interp_binop WasmTypeI64 WasmTypeI64 (-) stack))
    interp_instr mod frame stack labels after WasmInstrI64Mul = Right (frame, !(interp_binop WasmTypeI64 WasmTypeI64 (*) stack))
    interp_instr mod frame stack labels after WasmInstrI64Div_s = Right (frame, !(interp_binop WasmTypeI64 WasmTypeI64 div stack))
    interp_instr modu frame stack labels after WasmInstrI64Rem_s = Right (frame, !(interp_binop WasmTypeI64 WasmTypeI64 mod stack))
    interp_instr mod frame stack labels after WasmInstrI32And = Right (frame, !(interp_binop WasmTypeI32 WasmTypeI32 int_and stack))
    interp_instr mod frame stack labels after WasmInstrI32Or = Right (frame, !(interp_binop WasmTypeI32 WasmTypeI32 int_or stack))
    interp_instr mod frame stack labels after WasmInstrI32Eqz = Right (frame, !(interp_unop WasmTypeI32 WasmTypeI32 int_not stack))
    interp_instr mod frame stack labels after WasmInstrI32Eq = Right (frame, !(interp_binop WasmTypeI32 WasmTypeI32 (bool_to_int `comp2` (==)) stack))
    interp_instr mod frame stack labels after WasmInstrF64Add = Right (frame, !(interp_binop WasmTypeF64 WasmTypeF64 (+) stack))
    interp_instr mod frame stack labels after WasmInstrF64Sub = Right (frame, !(interp_binop WasmTypeF64 WasmTypeF64 (-) stack))
    interp_instr mod frame stack labels after WasmInstrF64Mul = Right (frame, !(interp_binop WasmTypeF64 WasmTypeF64 (*) stack))
    interp_instr mod frame stack labels after WasmInstrF64Div = Right (frame, !(interp_binop WasmTypeF64 WasmTypeF64 (/) stack))
    interp_instr mod frame stack labels after WasmInstrI64Eq = Right (frame, !(interp_binop WasmTypeI64 WasmTypeI32 (bool_to_int `comp2` (==)) stack))
    interp_instr mod frame stack labels after WasmInstrI64Lt_s = Right (frame, !(interp_binop WasmTypeI64 WasmTypeI32 (bool_to_int `comp2` (<)) stack))
    interp_instr mod frame stack labels after WasmInstrI64Gt_s = Right (frame, !(interp_binop WasmTypeI64 WasmTypeI32 (bool_to_int `comp2` (>)) stack))
    interp_instr mod frame stack labels after WasmInstrI64Le_s = Right (frame, !(interp_binop WasmTypeI64 WasmTypeI32 (bool_to_int `comp2` (<=)) stack))
    interp_instr mod frame stack labels after WasmInstrI64Ge_s = Right (frame, !(interp_binop WasmTypeI64 WasmTypeI32 (bool_to_int `comp2` (>=)) stack))
    interp_instr mod frame stack labels after WasmInstrF64Eq = Right (frame, !(interp_binop WasmTypeF64 WasmTypeI32 (bool_to_int `comp2` (==)) stack))
    interp_instr mod frame stack labels after WasmInstrF64Lt = Right (frame, !(interp_binop WasmTypeF64 WasmTypeI32 (bool_to_int `comp2` (<)) stack))
    interp_instr mod frame stack labels after WasmInstrF64Gt = Right (frame, !(interp_binop WasmTypeF64 WasmTypeI32 (bool_to_int `comp2` (>)) stack))
    interp_instr mod frame stack labels after WasmInstrF64Le = Right (frame, !(interp_binop WasmTypeF64 WasmTypeI32 (bool_to_int `comp2` (<=)) stack))
    interp_instr mod frame stack labels after WasmInstrF64Ge = Right (frame, !(interp_binop WasmTypeF64 WasmTypeI32 (bool_to_int `comp2` (>=)) stack))
    interp_instr mod frame stack labels after WasmInstrWrapI64ToI32 = Right (frame, !(interp_unop WasmTypeI64 WasmTypeI32 id stack))

    public export
    interp_instrs : (mod : WasmModule) -> (frame : State) -> (stack : List WasmValue) -> (labels : List Label) -> (instrs : List WasmInstr) -> Either String (State, List WasmValue)
    interp_instrs mod frame stack [] [] = Right (frame, stack)
    interp_instrs mod frame stack (l :: ls) [] = interp_instrs mod frame stack ls (continuation l) --Right (frame, stack)
    interp_instrs mod frame stack labels (i :: is) = do
        (frame', stack') <- interp_instr mod frame stack labels is i
        interp_instrs mod frame' stack' labels is

    public export
    interp_call : WasmModule -> WasmFunction -> List WasmValue -> Either String WasmValue
    interp_call mod (MkWasmFunction paramTypes resultType localTypes body id) args =
        let frame = args ++ alloc_types localTypes in do
        (frame', stack) <- interp_instrs mod frame [] [] body
        if length stack /= 1
            then Left "Expected function to return exactly 1 value"
            else case head' stack of
                Nothing => Left "Expected function to return exactly 1 value"
                (Just x) => Right x


public export
interp_module : WasmModule -> Either String WasmValue
interp_module mod@(MkWasmModule funcs start) = do
    start_f <- lookup_list start funcs
    if length (paramTypes start_f) /= 0
        then Left "Start function should take no arguments"
        else interp_call mod start_f []
