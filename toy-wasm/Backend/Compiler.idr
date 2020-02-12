module Compiler

import LangDefs.ToyAST
import LangDefs.WasmAST
import Backend.Optimizer

import Utils

import Data.Vect
import Data.Fin

%default covering

valueToWasmValue : (v : Value) -> Not (typeOfValue v = TypeUnit) -> WasmValue
valueToWasmValue (ValueInt x) prf = WasmValueI64 x
valueToWasmValue (ValueFloat x) prf = WasmValueF64 x
valueToWasmValue (ValueBool True) prf = WasmValueI32 1
valueToWasmValue (ValueBool False) prf = WasmValueI32 0
valueToWasmValue ValueUnit prf = void $ prf Refl

compile_type : (t : Type') -> Not (t = TypeUnit) -> WasmType
compile_type TypeInt prf = WasmTypeI64
compile_type TypeDouble prf = WasmTypeF64
compile_type TypeBool prf = WasmTypeI32
compile_type TypeUnit prf = void $ prf Refl

opt_compile_type : Type' -> Maybe WasmType
opt_compile_type TypeInt = Just WasmTypeI64
opt_compile_type TypeDouble = Just WasmTypeF64
opt_compile_type TypeBool = Just WasmTypeI32
opt_compile_type TypeUnit = Nothing

cast_instrs : (t : Type') -> (to_t : Type') -> List WasmInstr
cast_instrs TypeInt TypeDouble = [WasmInstrF64ConvertI64_s]
cast_instrs TypeInt TypeBool = [WasmInstrI64Eqz, WasmInstrI32Eqz]
cast_instrs TypeInt TypeUnit = [WasmInstrDrop]
cast_instrs TypeDouble TypeInt = [WasmInstrI64TruncF64_s]
cast_instrs TypeDouble TypeBool = [WasmInstrConst (WasmValueF64 0), WasmInstrF64Neq]
cast_instrs TypeDouble TypeUnit = [WasmInstrDrop]
cast_instrs TypeBool TypeInt = [WasmInstrI64ExtendI32_s]
cast_instrs TypeBool TypeDouble = [WasmInstrF64ConvertI32_s]
cast_instrs TypeBool TypeUnit = [WasmInstrDrop]
cast_instrs TypeUnit TypeInt = [WasmInstrConst (WasmValueI64 0)]
cast_instrs TypeUnit TypeDouble = [WasmInstrConst (WasmValueF64 0)]
cast_instrs TypeUnit TypeBool = [WasmInstrConst (WasmValueI32 0)]
cast_instrs TypeInt TypeInt = []
cast_instrs TypeDouble TypeDouble = []
cast_instrs TypeBool TypeBool = []
cast_instrs TypeUnit TypeUnit = []

lift_local_decls : Expr d fns -> List (t : Type' ** Not (t = TypeUnit))
lift_local_decls (ExprValue x) = []
lift_local_decls (ExprVar var) = []
lift_local_decls (ExprDeclareVar t not_unit initExpr after) = (t ** not_unit) :: (lift_local_decls initExpr ++ lift_local_decls after)
lift_local_decls (ExprUpdateVar var newExpr after) = lift_local_decls newExpr ++ lift_local_decls after
lift_local_decls (ExprCall f args) = foldl (\decls,arg => lift_local_decls arg ++ decls) [] args
lift_local_decls (ExprIf cond t true false) = lift_local_decls cond ++ lift_local_decls true ++ lift_local_decls false
lift_local_decls (ExprWhile cond body after) = lift_local_decls cond ++ lift_local_decls body ++ lift_local_decls after
lift_local_decls (ExprIAdd x y) = lift_local_decls x ++ lift_local_decls y
lift_local_decls (ExprFAdd x y) = lift_local_decls x ++ lift_local_decls y
lift_local_decls (ExprISub x y) = lift_local_decls x ++ lift_local_decls y
lift_local_decls (ExprFSub x y) = lift_local_decls x ++ lift_local_decls y
lift_local_decls (ExprIMul x y) = lift_local_decls x ++ lift_local_decls y
lift_local_decls (ExprFMul x y) = lift_local_decls x ++ lift_local_decls y
lift_local_decls (ExprIDiv x y) = lift_local_decls x ++ lift_local_decls y
lift_local_decls (ExprFDiv x y) = lift_local_decls x ++ lift_local_decls y
lift_local_decls (ExprIMod x y) = lift_local_decls x ++ lift_local_decls y
lift_local_decls (ExprIGT x y) = lift_local_decls x ++ lift_local_decls y
lift_local_decls (ExprFGT x y) = lift_local_decls x ++ lift_local_decls y
lift_local_decls (ExprIGTE x y) = lift_local_decls x ++ lift_local_decls y
lift_local_decls (ExprFGTE x y) = lift_local_decls x ++ lift_local_decls y
lift_local_decls (ExprIEQ x y) = lift_local_decls x ++ lift_local_decls y
lift_local_decls (ExprFEQ x y) = lift_local_decls x ++ lift_local_decls y
lift_local_decls (ExprILTE x y) = lift_local_decls x ++ lift_local_decls y
lift_local_decls (ExprFLTE x y) = lift_local_decls x ++ lift_local_decls y
lift_local_decls (ExprILT x y) = lift_local_decls x ++ lift_local_decls y
lift_local_decls (ExprFLT x y) = lift_local_decls x ++ lift_local_decls y
lift_local_decls (ExprAnd x y) = lift_local_decls x ++ lift_local_decls y
lift_local_decls (ExprOr x y) = lift_local_decls x ++ lift_local_decls y
lift_local_decls (ExprNot x) = lift_local_decls x
lift_local_decls (ExprINeg x) = lift_local_decls x
lift_local_decls (ExprFNeg x) = lift_local_decls x
lift_local_decls (ExprCast x t to_t) = lift_local_decls x

lift_function_locals : FuncDef fns -> List (t : Type' ** Not (t = TypeUnit))
lift_function_locals (MkFuncDef returnType argumentTypes body) = lift_local_decls body


is_small_int_expr : Expr d fns -> Bool
is_small_int_expr (ExprValue (ValueInt x)) = is_small_int x
is_small_int_expr e = False

is_small_float_expr : Expr d fns -> Bool
is_small_float_expr (ExprValue (ValueFloat x)) = is_small_float x
is_small_float_expr e = False

mutual
    compile_binop : (heap_stack : Bool) -> Vect fns (FuncDef fns) -> Fin fns -> Int -> (x : Expr d fns) -> (y : Expr d fns) -> WasmInstr -> (List WasmInstr, Int)
    compile_binop heap_stack fn_defs fn_idx numBound x y op =
        let (xins, numBound') = compile_expr heap_stack fn_defs fn_idx numBound x in
        let (yins, numBound'') = compile_expr heap_stack fn_defs fn_idx numBound' y in
        (xins ++ yins ++ [op], numBound'')

    compile_expr : (heap_stack : Bool) -> Vect fns (FuncDef fns) -> Fin fns -> Int -> Expr d fns -> (List WasmInstr, Int)
    compile_expr heap_stack fn_defs fn_idx numBound (ExprValue x) = case eq_unit (typeOfValue x) of
        (Yes prf) => ([], numBound)
        (No contra) => ([WasmInstrConst (valueToWasmValue x contra)], numBound)

    compile_expr heap_stack fn_defs fn_idx numBound (ExprVar var) =
        let local_id = (numBound - (toIntNat $ finToNat var) - 1) in
        if heap_stack then
            let param_types = argumentTypes (index fn_idx fn_defs) in
            let locals = lift_function_locals (index fn_idx fn_defs) in
            let Just (toy_local ** not_unit) = index' (toNat local_id) (param_types ++ locals)
                | Nothing => assert_unreachable -- hack, but ok for now.
            in
            let wasm_local_type = compile_type toy_local not_unit in
            ([WasmInstrLocalGet 0, WasmInstrLoad wasm_local_type (8*local_id)], numBound)
        else
            ([WasmInstrLocalGet local_id], numBound)

    compile_expr heap_stack fn_defs fn_idx numBound (ExprDeclareVar t not_unit initExpr after) =
        let (i_instrs, numBound') = compile_expr heap_stack fn_defs fn_idx numBound initExpr in
        let (a_instrs, numBound'') = compile_expr heap_stack fn_defs fn_idx (1 + numBound') after in
        if heap_stack then
            let wasm_local_type = compile_type t not_unit in
            ([WasmInstrLocalGet 0] ++ i_instrs ++ [WasmInstrStore wasm_local_type (8*numBound')] ++ a_instrs, numBound'')
        else
            (i_instrs ++ (WasmInstrLocalSet numBound' :: a_instrs), numBound'')

    compile_expr heap_stack fn_defs fn_idx numBound (ExprUpdateVar var newExpr after) =
        let (n_instrs, numBound') = compile_expr heap_stack fn_defs fn_idx numBound newExpr in
        let (a_instrs, numBound'') = compile_expr heap_stack fn_defs fn_idx numBound' after in
        let localSlot = numBound' - (toIntNat $ finToNat var) - 1 in
        if heap_stack then
            let param_types = argumentTypes (index fn_idx fn_defs) in
            let locals = lift_function_locals (index fn_idx fn_defs) in
            let Just (toy_local ** not_unit) = index' (toNat localSlot) (param_types ++ locals)
                | Nothing => assert_unreachable -- hack, but ok for now.
            in
            let wasm_local_type = compile_type toy_local not_unit in
            ([WasmInstrLocalGet 0] ++ n_instrs ++ [WasmInstrStore wasm_local_type (8*localSlot)] ++ a_instrs, numBound'')
        else
            (n_instrs ++ (WasmInstrLocalSet localSlot :: a_instrs), numBound'')

    compile_expr heap_stack fn_defs fn_idx numBound (ExprCall f args) =
        let f_def = index f fn_defs in
        let arg_types = argumentTypes f_def in
        let num_args = toIntNat (length args) in
        let num_locals = toIntNat (length (lift_function_locals f_def)) in
        let (args_ins, numBound', _) = foldl (\(instrs,b,i),(arg, (at ** not_unit)) =>
                                            let (ins, b') = compile_expr heap_stack fn_defs fn_idx b arg in
                                            if heap_stack then
                                                let wasm_at = compile_type at not_unit in
                                                ([
                                                    WasmInstrLocalGet 0,
                                                    WasmInstrConst (WasmValueI32 (8*(num_args + num_locals))),
                                                    WasmInstrI32Sub
                                                ] ++ ins ++ [WasmInstrStore wasm_at (8*i)] ++ instrs, b',i+1)
                                            else
                                                (ins ++ instrs, b',i+1)
                                    ) (the (List WasmInstr) [], numBound, the Int 0) (zip args arg_types) in
        if heap_stack then
            let read_ret =
            (
                case returnType f_def of
                    TypeInt => [WasmInstrConst (WasmValueI32 8), WasmInstrI32Sub, WasmInstrLoad WasmTypeI64 0]
                    TypeDouble => [WasmInstrConst (WasmValueI32 8), WasmInstrI32Sub, WasmInstrLoad WasmTypeF64 0]
                    TypeBool => [WasmInstrConst (WasmValueI32 8), WasmInstrI32Sub, WasmInstrLoad WasmTypeI32 0]
                    TypeUnit => [WasmInstrDrop]
            ) in

            (
                args_ins ++
                [
                    WasmInstrLocalGet 0,
                    WasmInstrConst (WasmValueI32 (8*(num_args + num_locals))),
                    WasmInstrI32Sub,
                    WasmInstrGlobalSet 0,
                    WasmInstrCall (toIntNat $ finToNat f),
                    WasmInstrGlobalGet 0,
                    WasmInstrLocalTee 0
                ] ++ read_ret, numBound')
        else
            (args_ins ++ [WasmInstrCall (toIntNat $ finToNat f)], numBound')

    compile_expr heap_stack fn_defs fn_idx numBound (ExprIf cond t true false) =
        let (cond_ins, numBound') = compile_expr heap_stack fn_defs fn_idx numBound cond in
        let (true_ins, numBound'') = compile_expr heap_stack fn_defs fn_idx numBound' true in
        let (false_ins, numBound''') = compile_expr heap_stack fn_defs fn_idx numBound'' false in
        (cond_ins ++ [WasmInstrIf (opt_compile_type t) true_ins false_ins], numBound''')

    compile_expr heap_stack fn_defs fn_idx numBound (ExprWhile cond body after) =
        let (cond_ins, numBound') = compile_expr heap_stack fn_defs fn_idx numBound cond in
        let (body_ins, numBound'') = compile_expr heap_stack fn_defs fn_idx numBound' body in
        let (after_ins, numBound''') = compile_expr heap_stack fn_defs fn_idx numBound'' after in
        (WasmInstrBlock Nothing (
            cond_ins ++ [WasmInstrI32Eqz, WasmInstrBrIf 0] ++
            [WasmInstrLoop Nothing (
                body_ins ++ cond_ins ++ [WasmInstrBrIf 0]
            )]
        ) :: after_ins, numBound''')

    compile_expr heap_stack fn_defs fn_idx numBound (ExprIAdd x y) = compile_binop heap_stack fn_defs fn_idx numBound x y WasmInstrI64Add
    compile_expr heap_stack fn_defs fn_idx numBound (ExprFAdd x y) = compile_binop heap_stack fn_defs fn_idx numBound x y WasmInstrF64Add
    compile_expr heap_stack fn_defs fn_idx numBound (ExprISub x y) = compile_binop heap_stack fn_defs fn_idx numBound x y WasmInstrI64Sub
    compile_expr heap_stack fn_defs fn_idx numBound (ExprFSub x y) = compile_binop heap_stack fn_defs fn_idx numBound x y WasmInstrF64Sub
    compile_expr heap_stack fn_defs fn_idx numBound (ExprINeg x) =
        let (xins, numBound') = compile_expr heap_stack fn_defs fn_idx numBound x in
        ([WasmInstrConst (WasmValueI64 0)] ++ xins ++ [WasmInstrI64Sub], numBound')
    compile_expr heap_stack fn_defs fn_idx numBound (ExprFNeg x) =
        let (xins, numBound') = compile_expr heap_stack fn_defs fn_idx numBound x in
        (xins ++ [WasmInstrF64Neg], numBound')
    compile_expr heap_stack fn_defs fn_idx numBound (ExprIMul x_tmp y_tmp) =
        if is_small_int_expr x_tmp
            then let (xins, numBound') = compile_expr heap_stack fn_defs fn_idx numBound x_tmp in
                 let (yins, numBound'') = compile_expr heap_stack fn_defs fn_idx numBound' y_tmp in
                 (yins ++ xins ++ [WasmInstrI64Mul], numBound'')
            else let (xins, numBound') = compile_expr heap_stack fn_defs fn_idx numBound x_tmp in
                 let (yins, numBound'') = compile_expr heap_stack fn_defs fn_idx numBound' y_tmp in
                 (xins ++ yins ++ [WasmInstrI64Mul], numBound'')
    compile_expr heap_stack fn_defs fn_idx numBound (ExprFMul x_tmp y_tmp) =
        if is_small_float_expr x_tmp
            then let (xins, numBound') = compile_expr heap_stack fn_defs fn_idx numBound x_tmp in
                 let (yins, numBound'') = compile_expr heap_stack fn_defs fn_idx numBound' y_tmp in
                 (yins ++ xins ++ [WasmInstrF64Mul], numBound'')
            else let (xins, numBound') = compile_expr heap_stack fn_defs fn_idx numBound x_tmp in
                 let (yins, numBound'') = compile_expr heap_stack fn_defs fn_idx numBound' y_tmp in
                 (xins ++ yins ++ [WasmInstrF64Mul], numBound'')
    compile_expr heap_stack fn_defs fn_idx numBound (ExprIDiv x y) = compile_binop heap_stack fn_defs fn_idx numBound x y WasmInstrI64Div_s
    compile_expr heap_stack fn_defs fn_idx numBound (ExprFDiv x y) = compile_binop heap_stack fn_defs fn_idx numBound x y WasmInstrF64Div
    compile_expr heap_stack fn_defs fn_idx numBound (ExprIMod x y) = compile_binop heap_stack fn_defs fn_idx numBound x y WasmInstrI64Rem_s
    compile_expr heap_stack fn_defs fn_idx numBound (ExprIGT x y) = compile_binop heap_stack fn_defs fn_idx numBound x y WasmInstrI64Gt_s
    compile_expr heap_stack fn_defs fn_idx numBound (ExprFGT x y) = compile_binop heap_stack fn_defs fn_idx numBound x y WasmInstrF64Gt
    compile_expr heap_stack fn_defs fn_idx numBound (ExprIGTE x y) = compile_binop heap_stack fn_defs fn_idx numBound x y WasmInstrI64Ge_s
    compile_expr heap_stack fn_defs fn_idx numBound (ExprFGTE x y) = compile_binop heap_stack fn_defs fn_idx numBound x y WasmInstrF64Ge
    compile_expr heap_stack fn_defs fn_idx numBound (ExprIEQ x y) = compile_binop heap_stack fn_defs fn_idx numBound x y WasmInstrI64Eq
    compile_expr heap_stack fn_defs fn_idx numBound (ExprFEQ x y) = compile_binop heap_stack fn_defs fn_idx numBound x y WasmInstrF64Eq
    compile_expr heap_stack fn_defs fn_idx numBound (ExprILTE x y) = compile_binop heap_stack fn_defs fn_idx numBound x y WasmInstrI64Le_s
    compile_expr heap_stack fn_defs fn_idx numBound (ExprFLTE x y) = compile_binop heap_stack fn_defs fn_idx numBound x y WasmInstrF64Le
    compile_expr heap_stack fn_defs fn_idx numBound (ExprILT x y) = compile_binop heap_stack fn_defs fn_idx numBound x y WasmInstrI64Lt_s
    compile_expr heap_stack fn_defs fn_idx numBound (ExprFLT x y) = compile_binop heap_stack fn_defs fn_idx numBound x y WasmInstrF64Lt
    compile_expr heap_stack fn_defs fn_idx numBound (ExprAnd x y) = compile_binop heap_stack fn_defs fn_idx numBound x y WasmInstrI32And
    compile_expr heap_stack fn_defs fn_idx numBound (ExprOr x y) = compile_binop heap_stack fn_defs fn_idx numBound x y WasmInstrI32Or
    compile_expr heap_stack fn_defs fn_idx numBound (ExprNot x) =
        let (xins, numBound') = compile_expr heap_stack fn_defs fn_idx numBound x in
        (xins ++ [WasmInstrI32Eqz], numBound')
    compile_expr heap_stack fn_defs fn_idx numBound (ExprCast x t to_t) =
        let (xins, numBound') = compile_expr heap_stack fn_defs fn_idx numBound x in
        (xins ++ cast_instrs t to_t, numBound')


compile_function : (heap_stack : Bool) -> Vect fns (FuncDef fns) -> Fin fns -> FuncDef fns -> WasmFunction
compile_function False fn_defs id (MkFuncDef returnType argumentTypes body) =
    let param_types = map (\(at ** not_unit) => compile_type at not_unit) argumentTypes in
    let ret_type = opt_compile_type returnType in
    let local_types = map (\(at ** not_unit) => compile_type at not_unit) (lift_local_decls body) in
    MkWasmFunction
        param_types
        ret_type
        local_types
        (fst (compile_expr False fn_defs id (toIntNat (length argumentTypes)) body))
        (toIntNat $ finToNat id)

{-
    Calling convention:
        - Immediately before (call $f) instruction:
            - arguments must have been pushed onto the stack,
            - and space for locals pushed onto stack. Stack looks like:
            | ...  |
            | ...  | <- old SP, stored in local 0
            | loc2 |
            | loc1 |
            | arg3 |
            | arg2 |
            | arg1 | <- SP, stored in global 0
            ... free stack space

        - Then, prologue for function $f must move SP, stored in WASM GLOBAL 0 to WASM LOCAL 0

        - Then, epilogue for function $f must pop the stack and write return value to stack, storing popped SP back in WASM GLOBAL 0.

        - Finally, the caller function must read the return value from just below the SP

-}
compile_function True fn_defs id f_def@(MkFuncDef returnType argumentTypes body) =
    let num_args = toIntNat (length argumentTypes) in
    let num_locals = toIntNat (length (lift_function_locals f_def)) in

    let prologue = [
        WasmInstrGlobalGet 0,
        WasmInstrLocalTee 0,
        WasmInstrConst (WasmValueI32 (8*(num_args + num_locals - 1))),
        WasmInstrI32Sub
    ] in

    let epilogue = [
        WasmInstrLocalGet 0,
        WasmInstrConst (WasmValueI32 (8*(num_args + num_locals))),
        WasmInstrI32Add,
        WasmInstrLocalTee 0,
        WasmInstrGlobalSet 0
    ] in
    let epilogue_ret_val : List WasmInstr =
    (
        case returnType of
            TypeInt => [WasmInstrStore WasmTypeI64 0]
            TypeDouble => [WasmInstrStore WasmTypeF64 0]
            TypeBool => [WasmInstrStore WasmTypeI32 0]
            TypeUnit => [WasmInstrDrop]
    ) in
    MkWasmFunction
        [] -- no parameters
        Nothing -- no return type
        [WasmTypeI32] -- one local: sp
        (
            prologue ++
            fst (compile_expr True fn_defs id (toIntNat (length argumentTypes)) body) ++
            epilogue ++ epilogue_ret_val
        )
        (toIntNat $ finToNat id)


export
compile_module : Bool -> Module nmfns -> WasmModule
compile_module heap_stack (MkModule functions) =
    let main_f = head functions in
    let main_ret_type = opt_compile_type $ returnType main_f in
    let main_locals = toIntNat (length (lift_function_locals main_f)) in

    let wasmFunctions = map_enum (compile_function heap_stack functions) functions in

    let call_instrs = [WasmInstrCall 0] in
    let cast_instrs : List WasmInstr = if main_ret_type == Just WasmTypeI64 then [WasmInstrWrapI64ToI32] else [] in
    let log_name = if main_ret_type == Just WasmTypeF64 then "log_f64" else "log_i32" in
    let log_instrs = if isJust main_ret_type then cast_instrs ++ [WasmInstrCallSpecial log_name] else [] in

    let read_ret_instrs =
    (
        case returnType main_f of
            TypeInt => [WasmInstrConst (WasmValueI32 8), WasmInstrI32Sub, WasmInstrLoad WasmTypeI64 0]
            TypeDouble => [WasmInstrConst (WasmValueI32 8), WasmInstrI32Sub, WasmInstrLoad WasmTypeF64 0]
            TypeBool => [WasmInstrConst (WasmValueI32 8), WasmInstrI32Sub, WasmInstrLoad WasmTypeI32 0]
            TypeUnit => [WasmInstrDrop]
    ) in

    let wasm_start_body = if heap_stack then
        [
            WasmInstrConst (WasmValueI32 (1024 - 8*main_locals)),
            WasmInstrGlobalSet 0
        ] ++ call_instrs ++ [WasmInstrGlobalGet 0] ++ read_ret_instrs ++ log_instrs
    else
        call_instrs ++ log_instrs
    in

    let wasm_start_f = MkWasmFunction [] Nothing [] wasm_start_body (toIntNat $ length wasmFunctions) in

    let globals = if heap_stack then [(True, WasmValueI32 0)] else [] in
    let memory = if heap_stack then Just 1 else Nothing in

    MkWasmModule
        (toList wasmFunctions ++ [wasm_start_f])
        (toIntNat $ length wasmFunctions)
        [
            MkWasmFunctionImport "console" "log_i32" "log_i32" [WasmTypeI32] Nothing,
            MkWasmFunctionImport "console" "log_f64" "log_f64" [WasmTypeF64] Nothing
        ]
        globals
        memory

-- export
-- compile_module : Bool -> Module nmfns -> WasmModule
-- compile_module optim m = optimize_module optim (compile_module' m)
