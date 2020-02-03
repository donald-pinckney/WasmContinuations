module WasmDump

import LangDefs.WasmAST

%default covering

join_by : List String -> String -> String
join_by [] j = ""
join_by [x] j = x
join_by (x :: xs) j = x ++ j ++ (join_by xs j)

map_enum : Int -> (Int -> a -> b) -> List a -> List b
map_enum acc f [] = []
map_enum acc f (x :: xs) = f acc x :: map_enum (acc + 1) f xs

dump_type : WasmType -> String
dump_type WasmTypeI64 = "i64"
dump_type WasmTypeF64 = "f64"
dump_type WasmTypeI32 = "i32"

tab_str : Nat -> String
tab_str k = pack (replicate k '\t')

mutual
    dump_instr : Nat -> WasmInstr -> String
    dump_instr tab (WasmInstrConst (WasmValueI64 x)) = (tab_str tab) ++ "i64.const " ++ show x
    dump_instr tab (WasmInstrConst (WasmValueF64 x)) = (tab_str tab) ++ "f64.const " ++ show x
    dump_instr tab (WasmInstrConst (WasmValueI32 x)) = (tab_str tab) ++ "i32.const " ++ show x
    dump_instr tab WasmInstrDrop = (tab_str tab) ++ "drop"
    dump_instr tab (WasmInstrLocalGet x) = (tab_str tab) ++ "get_local " ++ show x
    dump_instr tab (WasmInstrLocalSet x) = (tab_str tab) ++ "set_local " ++ show x
    dump_instr tab (WasmInstrLocalTee x) = (tab_str tab) ++ "tee_local " ++ show x
    dump_instr tab (WasmInstrBlock Nothing xs) = (tab_str tab) ++ "(block\n" ++
                                                dump_instrs (S tab) xs ++
                                            "\n" ++ (tab_str tab) ++ ")"
    dump_instr tab (WasmInstrBlock (Just t) xs) = (tab_str tab) ++ "(block (result " ++ dump_type t ++ ")\n" ++
                                                dump_instrs (S tab) xs ++
                                            "\n" ++ (tab_str tab) ++ ")"
    dump_instr tab (WasmInstrIf Nothing xs ys) = (tab_str tab) ++ "(if\n" ++
                                                (tab_str (S tab)) ++ "(then\n" ++ dump_instrs (S (S tab)) xs ++
                                                "\n" ++ (tab_str (S tab)) ++ ")\n" ++ (tab_str (S tab)) ++ "(else\n" ++
                                                    dump_instrs (S (S tab)) ys ++ "\n" ++ (tab_str (S tab)) ++ ")\n" ++ (tab_str tab) ++ ")"
    dump_instr tab (WasmInstrIf (Just t) xs ys) = (tab_str tab) ++ "(if (result " ++ dump_type t ++ ")\n" ++
                                                (tab_str (S tab)) ++ "(then\n" ++ dump_instrs (S (S tab)) xs ++
                                                "\n" ++ (tab_str (S tab)) ++ ")\n" ++ (tab_str (S tab)) ++ "(else\n" ++
                                                    dump_instrs (S (S tab)) ys ++ "\n" ++ (tab_str (S tab)) ++ ")\n" ++ (tab_str tab) ++ ")"
    dump_instr tab (WasmInstrLoop Nothing xs) = (tab_str tab) ++ "(loop\n" ++
                                                dump_instrs (S tab) xs ++
                                            "\n" ++ (tab_str tab) ++ ")"
    dump_instr tab (WasmInstrLoop (Just t) xs) = (tab_str tab) ++ "(loop (result " ++ dump_type t ++ ")\n" ++
                                                dump_instrs (S tab) xs ++
                                            "\n" ++ (tab_str tab) ++ ")"
    dump_instr tab (WasmInstrBr x) = (tab_str tab) ++ "br " ++ show x
    dump_instr tab (WasmInstrBrIf x) = (tab_str tab) ++ "br_if " ++ show x
    dump_instr tab (WasmInstrCall x) = (tab_str tab) ++ "call $f" ++ show x
    dump_instr tab WasmInstrI64Add = (tab_str tab) ++ "i64.add"
    dump_instr tab WasmInstrI64Sub = (tab_str tab) ++ "i64.sub"
    dump_instr tab WasmInstrI64Mul = (tab_str tab) ++ "i64.mul"
    dump_instr tab WasmInstrI64Div_s = (tab_str tab) ++ "i64.div_s"
    dump_instr tab WasmInstrI64Rem_s = (tab_str tab) ++ "i64.rem_s"
    dump_instr tab WasmInstrI32And = (tab_str tab) ++ "i32.and"
    dump_instr tab WasmInstrI32Or = (tab_str tab) ++ "i32.or"
    dump_instr tab WasmInstrI32Eqz = (tab_str tab) ++ "i32.eqz"
    dump_instr tab WasmInstrI32Eq = (tab_str tab) ++ "i32.eq"
    dump_instr tab WasmInstrF64Add = (tab_str tab) ++ "f64.add"
    dump_instr tab WasmInstrF64Sub = (tab_str tab) ++ "f64.sub"
    dump_instr tab WasmInstrF64Mul = (tab_str tab) ++ "f64.mul"
    dump_instr tab WasmInstrF64Div = (tab_str tab) ++ "f64.div"
    dump_instr tab WasmInstrI64Eq = (tab_str tab) ++ "i64.eq"
    dump_instr tab WasmInstrI64Lt_s = (tab_str tab) ++ "i64.lt_s"
    dump_instr tab WasmInstrI64Gt_s = (tab_str tab) ++ "i64.gt_s"
    dump_instr tab WasmInstrI64Le_s = (tab_str tab) ++ "i64.le_s"
    dump_instr tab WasmInstrI64Ge_s = (tab_str tab) ++ "i64.ge_s"
    dump_instr tab WasmInstrF64Eq = (tab_str tab) ++ "f64.eq"
    dump_instr tab WasmInstrF64Lt = (tab_str tab) ++ "f64.lt"
    dump_instr tab WasmInstrF64Gt = (tab_str tab) ++ "f64.gt"
    dump_instr tab WasmInstrF64Le = (tab_str tab) ++ "f64.le"
    dump_instr tab WasmInstrF64Ge = (tab_str tab) ++ "f64.ge"
    dump_instr tab WasmInstrWrapI64ToI32 = (tab_str tab) ++ "i32.wrap_i64"
    dump_instr tab WasmInstrI64Shr_u = (tab_str tab) ++ "i64.shr_u"
    dump_instr tab WasmInstrI64Shl = (tab_str tab) ++ "i64.shl"
    dump_instr tab WasmInstrI64And = (tab_str tab) ++ "i64.and"
    dump_instr tab WasmInstrI64Neq = (tab_str tab) ++ "i64.ne"
    dump_instr tab WasmInstrI64Eqz = (tab_str tab) ++ "i64.eqz"
    dump_instr tab WasmInstrF64Neg = (tab_str tab) ++ "f64.neg"
    dump_instr tab WasmInstrI64ExtendI32_s = (tab_str tab) ++ "i64.extend_i32_s"
    dump_instr tab WasmInstrI64TruncF64_s = (tab_str tab) ++ "i64.trunc_f64_s"
    dump_instr tab WasmInstrF64ConvertI32_s = (tab_str tab) ++ "f64.convert_i32_s"
    dump_instr tab WasmInstrF64ConvertI64_s = (tab_str tab) ++ "f64.convert_i64_s"
    dump_instr tab WasmInstrF64Neq = (tab_str tab) ++ "f64.ne"


    dump_instrs : Nat -> List WasmInstr -> String
    dump_instrs indent xs = join_by (map (dump_instr indent) xs) "\n"

dump_function : WasmFunction -> String
dump_function (MkWasmFunction paramTypes resultType localTypes body id) =
    let paramsString = join_by (map (\t => "(param " ++ dump_type t ++ ")" ) paramTypes) " " in
    let localsString = join_by (map (\t => "(local " ++ dump_type t ++ ")" ) localTypes) " " in
    "\t(func $f" ++ show id ++ " " ++ paramsString ++ " (result " ++ dump_type resultType ++ " ) " ++ localsString ++ "\n" ++
        dump_instrs 2 body ++
    "\n\t)"

export
dump_module : WasmModule -> String
dump_module (MkWasmModule funcs start st) =
    let st' = if st == WasmTypeI64 then WasmTypeI32 else st in
    "(module\n" ++
        "\t(import \"console\" \"log_i32\" (func $log_i32 (param i32)))\n" ++
        "\t(import \"console\" \"log_f64\" (func $log_f64 (param f64)))\n" ++
        -- "\t(import \"console\" \"log_i64\" (func $log_i64 (param i64)))\n" ++
        (join_by (map dump_function funcs) "\n") ++ "\n" ++
        "\t(func $start\n\t\tcall $f" ++ show start ++ "\n\t\t" ++ (if st == WasmTypeI64 then "i32.wrap_i64\n\t\t" else "") ++ "call $log_" ++ dump_type st' ++ "\n\t)\n" ++
        "\t(start $start)\n" ++
    ")"
