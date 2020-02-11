module Compiler

import LangDefs.ToyAST
import LangDefs.WasmAST
import Backend.Optimizer

import Data.Vect
import Data.Fin

%default covering

map_enum : Int -> (Int -> a -> b) -> List a -> List b
map_enum acc f [] = []
map_enum acc f (x :: xs) = f acc x :: map_enum (acc + 1) f xs

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

is_small_int_expr : Expr d fns -> Bool
is_small_int_expr (ExprValue (ValueInt x)) = is_small_int x
is_small_int_expr e = False

is_small_float_expr : Expr d fns -> Bool
is_small_float_expr (ExprValue (ValueFloat x)) = is_small_float x
is_small_float_expr e = False

mutual
    compile_binop : (heap_stack : Bool) -> Vect fns (List (t : Type' ** Not (t = TypeUnit))) -> Int -> (x : Expr d fns) -> (y : Expr d fns) -> WasmInstr -> (List WasmInstr, Int)
    compile_binop heap_stack fn_locals numBound x y op =
        let (xins, numBound') = compile_expr heap_stack fn_locals numBound x in
        let (yins, numBound'') = compile_expr heap_stack fn_locals numBound' y in
        (xins ++ yins ++ [op], numBound'')

    compile_expr : (heap_stack : Bool) -> Vect fns (List (t : Type' ** Not (t = TypeUnit))) -> Int -> Expr d fns -> (List WasmInstr, Int)
    compile_expr heap_stack fn_locals numBound (ExprValue x) = case eq_unit (typeOfValue x) of
        (Yes prf) => ([], numBound)
        (No contra) => ([WasmInstrConst (valueToWasmValue x contra)], numBound)

    compile_expr heap_stack fn_locals numBound (ExprVar var) =
        if heap_stack then
            ?read_var
        else
            ([WasmInstrLocalGet (numBound - (toIntNat $ finToNat var) - 1)], numBound)

    compile_expr heap_stack fn_locals numBound (ExprDeclareVar t not_unit initExpr after) =
        let (i_instrs, numBound') = compile_expr heap_stack fn_locals numBound initExpr in
        let (a_instrs, numBound'') = compile_expr heap_stack fn_locals (1 + numBound') after in
        if heap_stack then
            ?declare_var
        else
            (i_instrs ++ (WasmInstrLocalSet numBound' :: a_instrs), numBound'')

    compile_expr heap_stack fn_locals numBound (ExprUpdateVar var newExpr after) =
        let (n_instrs, numBound') = compile_expr heap_stack fn_locals numBound newExpr in
        let (a_instrs, numBound'') = compile_expr heap_stack fn_locals numBound' after in
        let localSlot = numBound' - (toIntNat $ finToNat var) - 1 in
        if heap_stack then
            ?update_var
        else
            (n_instrs ++ (WasmInstrLocalSet localSlot :: a_instrs), numBound'')

    compile_expr heap_stack fn_locals numBound (ExprCall f args) =
        let (args_ins, numBound') = foldl (\(instrs,b),arg =>
                                            let (ins, b') = compile_expr heap_stack fn_locals b arg in
                                            (ins ++ instrs, b')
                                    ) (the (List WasmInstr) [], numBound) args in
        if heap_stack then
            ?do_call
        else
            (args_ins ++ [WasmInstrCall (toIntNat $ finToNat f)], numBound')

    compile_expr heap_stack fn_locals numBound (ExprIf cond t true false) =
        let (cond_ins, numBound') = compile_expr heap_stack fn_locals numBound cond in
        let (true_ins, numBound'') = compile_expr heap_stack fn_locals numBound' true in
        let (false_ins, numBound''') = compile_expr heap_stack fn_locals numBound'' false in
        (cond_ins ++ [WasmInstrIf (opt_compile_type t) true_ins false_ins], numBound''')

    compile_expr heap_stack fn_locals numBound (ExprWhile cond body after) =
        let (cond_ins, numBound') = compile_expr heap_stack fn_locals numBound cond in
        let (body_ins, numBound'') = compile_expr heap_stack fn_locals numBound' body in
        let (after_ins, numBound''') = compile_expr heap_stack fn_locals numBound'' after in
        (WasmInstrBlock Nothing (
            cond_ins ++ [WasmInstrI32Eqz, WasmInstrBrIf 0] ++
            [WasmInstrLoop Nothing (
                body_ins ++ cond_ins ++ [WasmInstrBrIf 0]
            )]
        ) :: after_ins, numBound''')

    compile_expr heap_stack fn_locals numBound (ExprIAdd x y) = compile_binop heap_stack fn_locals numBound x y WasmInstrI64Add
    compile_expr heap_stack fn_locals numBound (ExprFAdd x y) = compile_binop heap_stack fn_locals numBound x y WasmInstrF64Add
    compile_expr heap_stack fn_locals numBound (ExprISub x y) = compile_binop heap_stack fn_locals numBound x y WasmInstrI64Sub
    compile_expr heap_stack fn_locals numBound (ExprFSub x y) = compile_binop heap_stack fn_locals numBound x y WasmInstrF64Sub
    compile_expr heap_stack fn_locals numBound (ExprINeg x) =
        let (xins, numBound') = compile_expr heap_stack fn_locals numBound x in
        ([WasmInstrConst (WasmValueI64 0)] ++ xins ++ [WasmInstrI64Sub], numBound')
    compile_expr heap_stack fn_locals numBound (ExprFNeg x) =
        let (xins, numBound') = compile_expr heap_stack fn_locals numBound x in
        (xins ++ [WasmInstrF64Neg], numBound')
    compile_expr heap_stack fn_locals numBound (ExprIMul x_tmp y_tmp) =
        if is_small_int_expr x_tmp
            then let (xins, numBound') = compile_expr heap_stack fn_locals numBound x_tmp in
                 let (yins, numBound'') = compile_expr heap_stack fn_locals numBound' y_tmp in
                 (yins ++ xins ++ [WasmInstrI64Mul], numBound'')
            else let (xins, numBound') = compile_expr heap_stack fn_locals numBound x_tmp in
                 let (yins, numBound'') = compile_expr heap_stack fn_locals numBound' y_tmp in
                 (xins ++ yins ++ [WasmInstrI64Mul], numBound'')
    compile_expr heap_stack fn_locals numBound (ExprFMul x_tmp y_tmp) =
        if is_small_float_expr x_tmp
            then let (xins, numBound') = compile_expr heap_stack fn_locals numBound x_tmp in
                 let (yins, numBound'') = compile_expr heap_stack fn_locals numBound' y_tmp in
                 (yins ++ xins ++ [WasmInstrF64Mul], numBound'')
            else let (xins, numBound') = compile_expr heap_stack fn_locals numBound x_tmp in
                 let (yins, numBound'') = compile_expr heap_stack fn_locals numBound' y_tmp in
                 (xins ++ yins ++ [WasmInstrF64Mul], numBound'')
    compile_expr heap_stack fn_locals numBound (ExprIDiv x y) = compile_binop heap_stack fn_locals numBound x y WasmInstrI64Div_s
    compile_expr heap_stack fn_locals numBound (ExprFDiv x y) = compile_binop heap_stack fn_locals numBound x y WasmInstrF64Div
    compile_expr heap_stack fn_locals numBound (ExprIMod x y) = compile_binop heap_stack fn_locals numBound x y WasmInstrI64Rem_s
    compile_expr heap_stack fn_locals numBound (ExprIGT x y) = compile_binop heap_stack fn_locals numBound x y WasmInstrI64Gt_s
    compile_expr heap_stack fn_locals numBound (ExprFGT x y) = compile_binop heap_stack fn_locals numBound x y WasmInstrF64Gt
    compile_expr heap_stack fn_locals numBound (ExprIGTE x y) = compile_binop heap_stack fn_locals numBound x y WasmInstrI64Ge_s
    compile_expr heap_stack fn_locals numBound (ExprFGTE x y) = compile_binop heap_stack fn_locals numBound x y WasmInstrF64Ge
    compile_expr heap_stack fn_locals numBound (ExprIEQ x y) = compile_binop heap_stack fn_locals numBound x y WasmInstrI64Eq
    compile_expr heap_stack fn_locals numBound (ExprFEQ x y) = compile_binop heap_stack fn_locals numBound x y WasmInstrF64Eq
    compile_expr heap_stack fn_locals numBound (ExprILTE x y) = compile_binop heap_stack fn_locals numBound x y WasmInstrI64Le_s
    compile_expr heap_stack fn_locals numBound (ExprFLTE x y) = compile_binop heap_stack fn_locals numBound x y WasmInstrF64Le
    compile_expr heap_stack fn_locals numBound (ExprILT x y) = compile_binop heap_stack fn_locals numBound x y WasmInstrI64Lt_s
    compile_expr heap_stack fn_locals numBound (ExprFLT x y) = compile_binop heap_stack fn_locals numBound x y WasmInstrF64Lt
    compile_expr heap_stack fn_locals numBound (ExprAnd x y) = compile_binop heap_stack fn_locals numBound x y WasmInstrI32And
    compile_expr heap_stack fn_locals numBound (ExprOr x y) = compile_binop heap_stack fn_locals numBound x y WasmInstrI32Or
    compile_expr heap_stack fn_locals numBound (ExprNot x) =
        let (xins, numBound') = compile_expr heap_stack fn_locals numBound x in
        (xins ++ [WasmInstrI32Eqz], numBound')
    compile_expr heap_stack fn_locals numBound (ExprCast x t to_t) =
        let (xins, numBound') = compile_expr heap_stack fn_locals numBound x in
        (xins ++ cast_instrs t to_t, numBound')

lift_function_locals : FuncDef fns -> List (t : Type' ** Not (t = TypeUnit))
lift_function_locals (MkFuncDef returnType argumentTypes body) = lift_local_decls body

compile_function : (heap_stack : Bool) -> Vect fns (List (t : Type' ** Not (t = TypeUnit))) -> Int -> FuncDef fns -> WasmFunction
compile_function False fn_locals id (MkFuncDef returnType argumentTypes body) =
    let param_types = map (\(at ** not_unit) => compile_type at not_unit) argumentTypes in
    let ret_type = opt_compile_type returnType in
    let local_types = map (\(at ** not_unit) => compile_type at not_unit) (lift_local_decls body) in
    MkWasmFunction
        param_types
        ret_type
        local_types
        (fst (compile_expr False fn_locals (toIntNat (length argumentTypes)) body))
        id

{-
    Calling convention:
        - Immediately before (call $f) instruction:
            - arguments must have been pushed onto the stack,
            - and space for locals pushed onto stack. Stack looks like:
            | ...  |
            | ...  |
            | loc2 |
            | loc1 |
            | arg3 |
            | arg2 |
            | arg1 | <- SP, stored in global 0
            ... free stack space

        - Then, prologue for function $f must move SP, stored in WASM GLOBAL 0 to WASM LOCAL 0

-}
compile_function True fn_locals id (MkFuncDef returnType argumentTypes body) =
    -- ?opuwerwe
    MkWasmFunction
        [] -- no parameters
        Nothing -- no return type
        [WasmTypeI32] -- one local: sp
        ([WasmInstrGlobalGet 0, WasmInstrLocalSet 0] ++ fst (compile_expr True fn_locals (toIntNat (length argumentTypes)) body))
        id

export
compile_module : Bool -> Module nmfns -> WasmModule
compile_module heap_stack (MkModule functions) =
    let fn_locals = map lift_function_locals functions in
    let main_f = head functions in
    let wasmFunctions = map_enum 0 (compile_function heap_stack fn_locals) (toList functions) in
    MkWasmModule wasmFunctions 0 (opt_compile_type $ returnType main_f)


-- export
-- compile_module : Bool -> Module nmfns -> WasmModule
-- compile_module optim m = optimize_module optim (compile_module' m)
