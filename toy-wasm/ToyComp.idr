module ToyComp

import ToyAST
import WasmAST
import ToyInterp
import WasmInterp
import Data.Vect
import Data.Fin

%default covering

map_enum : Int -> (Int -> a -> b) -> List a -> List b
map_enum acc f [] = []
map_enum acc f (x :: xs) = f acc x :: map_enum (acc + 1) f xs

valueToWasmValue : Value -> WasmValue
valueToWasmValue (ValueInt x) = WasmValueI64 x
valueToWasmValue (ValueFloat x) = WasmValueF64 x
valueToWasmValue (ValueBool True) = WasmValueI32 1
valueToWasmValue (ValueBool False) = WasmValueI32 0

compile_type : Type' -> WasmType
compile_type TypeInt = WasmTypeI64
compile_type TypeDouble = WasmTypeF64
compile_type TypeBool = WasmTypeI32


lift_local_decls : Expr d fns -> List Type'
lift_local_decls (ExprValue x) = []
lift_local_decls (ExprVar var) = []
lift_local_decls (ExprDeclareVar t initExpr after) = t :: (lift_local_decls initExpr ++ lift_local_decls after)
lift_local_decls (ExprUpdateVar var newExpr after) = lift_local_decls newExpr ++ lift_local_decls after
lift_local_decls (ExprCall f args) = foldl (\decls,arg => lift_local_decls arg ++ decls) [] args
lift_local_decls (ExprIf cond true false) = lift_local_decls cond ++ lift_local_decls true ++ lift_local_decls false
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

compile_expr : Int -> Expr d fns -> (List WasmInstr, Int)
compile_expr numBound (ExprValue x) = ([WasmInstrConst (valueToWasmValue x)], numBound)
compile_expr numBound (ExprVar var) = ([WasmInstrLocalGet (numBound - (toIntNat $ finToNat var) - 1)], numBound)
compile_expr numBound (ExprDeclareVar t initExpr after) =
    let (i_instrs, numBound') = compile_expr numBound initExpr in
    let (a_instrs, numBound'') = compile_expr (1 + numBound') after in
    (i_instrs ++ (WasmInstrLocalSet numBound' :: a_instrs), numBound'')
compile_expr numBound (ExprUpdateVar var newExpr after) =
    let (n_instrs, numBound') = compile_expr numBound newExpr in
    let (a_instrs, numBound'') = compile_expr numBound' after in
    (n_instrs ++ (WasmInstrLocalSet (numBound' - (toIntNat $ finToNat var) - 1) :: a_instrs), numBound'')
compile_expr numBound (ExprCall f args) = foldl (\(instrs,b),arg =>
                                                        let (ins, b') = compile_expr b arg in
                                                        (ins ++ instrs, b')
                                                ) ([], numBound) args
compile_expr numBound (ExprIf cond true false) = ?compile_expr_rhs_6
compile_expr numBound (ExprWhile cond body after) = ?compile_expr_rhs_7
compile_expr numBound (ExprIAdd x y) =
    let (xins, numBound') = compile_expr numBound x in
    let (yins, numBound'') = compile_expr numBound' y in
    (xins ++ yins ++ [WasmInstrI64Add], numBound'')
compile_expr numBound (ExprFAdd x y) =
    let (xins, numBound') = compile_expr numBound x in
    let (yins, numBound'') = compile_expr numBound' y in
    (xins ++ yins ++ [WasmInstrF64Add], numBound'')
compile_expr numBound (ExprISub x y) =
    let (xins, numBound') = compile_expr numBound x in
    let (yins, numBound'') = compile_expr numBound' y in
    (xins ++ yins ++ [WasmInstrI64Sub], numBound'')
compile_expr numBound (ExprFSub x y) =
    let (xins, numBound') = compile_expr numBound x in
    let (yins, numBound'') = compile_expr numBound' y in
    (xins ++ yins ++ [WasmInstrF64Sub], numBound'')
compile_expr numBound (ExprIMul x y) =
    let (xins, numBound') = compile_expr numBound x in
    let (yins, numBound'') = compile_expr numBound' y in
    (xins ++ yins ++ [WasmInstrI64Mul], numBound'')
compile_expr numBound (ExprFMul x y) =
    let (xins, numBound') = compile_expr numBound x in
    let (yins, numBound'') = compile_expr numBound' y in
    (xins ++ yins ++ [WasmInstrF64Mul], numBound'')
compile_expr numBound (ExprIDiv x y) =
    let (xins, numBound') = compile_expr numBound x in
    let (yins, numBound'') = compile_expr numBound' y in
    (xins ++ yins ++ [WasmInstrI64Div_s], numBound'')
compile_expr numBound (ExprFDiv x y) =
    let (xins, numBound') = compile_expr numBound x in
    let (yins, numBound'') = compile_expr numBound' y in
    (xins ++ yins ++ [WasmInstrF64Div], numBound'')
compile_expr numBound (ExprIMod x y) =
    let (xins, numBound') = compile_expr numBound x in
    let (yins, numBound'') = compile_expr numBound' y in
    (xins ++ yins ++ [WasmInstrI64Rem_s], numBound'')
compile_expr numBound (ExprIGT x y) =
    let (xins, numBound') = compile_expr numBound x in
    let (yins, numBound'') = compile_expr numBound' y in
    (xins ++ yins ++ [WasmInstrI64Gt_s], numBound'')
compile_expr numBound (ExprFGT x y) =
    let (xins, numBound') = compile_expr numBound x in
    let (yins, numBound'') = compile_expr numBound' y in
    (xins ++ yins ++ [WasmInstrF64Gt], numBound'')
compile_expr numBound (ExprIGTE x y) =
    let (xins, numBound') = compile_expr numBound x in
    let (yins, numBound'') = compile_expr numBound' y in
    (xins ++ yins ++ [WasmInstrI64Ge_s], numBound'')
compile_expr numBound (ExprFGTE x y) =
    let (xins, numBound') = compile_expr numBound x in
    let (yins, numBound'') = compile_expr numBound' y in
    (xins ++ yins ++ [WasmInstrF64Ge], numBound'')
compile_expr numBound (ExprIEQ x y) =
    let (xins, numBound') = compile_expr numBound x in
    let (yins, numBound'') = compile_expr numBound' y in
    (xins ++ yins ++ [WasmInstrI64Eq], numBound'')
compile_expr numBound (ExprFEQ x y) =
    let (xins, numBound') = compile_expr numBound x in
    let (yins, numBound'') = compile_expr numBound' y in
    (xins ++ yins ++ [WasmInstrF64Eq], numBound'')
compile_expr numBound (ExprILTE x y) =
    let (xins, numBound') = compile_expr numBound x in
    let (yins, numBound'') = compile_expr numBound' y in
    (xins ++ yins ++ [WasmInstrI64Le_s], numBound'')
compile_expr numBound (ExprFLTE x y) =
    let (xins, numBound') = compile_expr numBound x in
    let (yins, numBound'') = compile_expr numBound' y in
    (xins ++ yins ++ [WasmInstrF64Le], numBound'')
compile_expr numBound (ExprILT x y) =
    let (xins, numBound') = compile_expr numBound x in
    let (yins, numBound'') = compile_expr numBound' y in
    (xins ++ yins ++ [WasmInstrI64Lt_s], numBound'')
compile_expr numBound (ExprFLT x y) =
    let (xins, numBound') = compile_expr numBound x in
    let (yins, numBound'') = compile_expr numBound' y in
    (xins ++ yins ++ [WasmInstrF64Lt], numBound'')
compile_expr numBound (ExprAnd x y) =
    let (xins, numBound') = compile_expr numBound x in
    let (yins, numBound'') = compile_expr numBound' y in
    (xins ++ yins ++ [WasmInstrI32And], numBound'')
compile_expr numBound (ExprOr x y) =
    let (xins, numBound') = compile_expr numBound x in
    let (yins, numBound'') = compile_expr numBound' y in
    (xins ++ yins ++ [WasmInstrI32Or], numBound'')
compile_expr numBound (ExprNot x) =
    let (xins, numBound') = compile_expr numBound x in
    (xins ++ [WasmInstrI32Eqz], numBound')

compile_function : Int -> FuncDef fns -> WasmFunction
compile_function id (MkFuncDef returnType argumentTypes body) =
    MkWasmFunction
        (map compile_type argumentTypes)
        (compile_type returnType)
        (map compile_type (lift_local_decls body))
        -- ?pouiwerwe
        (fst (compile_expr (toIntNat (length argumentTypes)) body))
        id

export
compile_module : Module nmfns -> WasmModule
compile_module (MkModule functions) =
    let main_f = head functions in
    let wasmFunctions = map_enum 0 compile_function (toList functions) in
    MkWasmModule wasmFunctions 0 (compile_type $ returnType main_f)
