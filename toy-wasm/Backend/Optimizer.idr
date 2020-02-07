module Optimizer

import LangDefs.WasmAST
import Utils

%default covering

RewriteRule : Type
RewriteRule = List WasmInstr -> Maybe (List WasmInstr)

exactRule : (List WasmInstr, List WasmInstr) -> RewriteRule
exactRule (pat, new) = \instrs => if pat == instrs then Just new else Nothing

constDropRule : RewriteRule
constDropRule [WasmInstrConst v, WasmInstrDrop] = Just []
constDropRule instrs = Nothing

teeRule : RewriteRule
teeRule [WasmInstrLocalSet l1, WasmInstrLocalGet l2] = if l1 == l2 then Just [WasmInstrLocalTee l1] else Nothing
teeRule instrs = Nothing

mulAddRule : RewriteRule
mulAddRule [WasmInstrLocalGet l, WasmInstrConst (WasmValueF64 v), WasmInstrF64Mul] = if v == 2.0 then Just [WasmInstrLocalGet l, WasmInstrLocalGet l, WasmInstrF64Add] else Nothing
mulAddRule instrs = Nothing

export
is_small_int : Int -> Bool
is_small_int x = x <= 5

export
is_small_float : Double -> Bool
is_small_float x = x <= 5

-- smallMulRule : RewriteRule
-- smallMulRule [WasmInstrConst (WasmValueI64 n), WasmInstrI64Mul] = if n == 0 || n == 1 || not (is_small_int n)
--     then Nothing
--     else ?eNothing

Rules : List RewriteRule
Rules =
    map (\bits => exactRule ([WasmInstrConst (WasmValueI64 (shiftL 1 bits)), WasmInstrI64Div_s],
                   [WasmInstrConst (WasmValueI64 bits), WasmInstrI64Shr_u])
        ) (enumFromTo 1 62) ++
    map (\bits => exactRule ([WasmInstrConst (WasmValueI64 (shiftL 1 bits)), WasmInstrI64Mul],
                   [WasmInstrConst (WasmValueI64 bits), WasmInstrI64Shl])
        ) (enumFromTo 1 62) ++
    map (\bits => exactRule ([WasmInstrConst (WasmValueI64 (shiftL 1 bits)), WasmInstrI64Rem_s],
               [WasmInstrConst (WasmValueI64 (shiftL 1 (bits - 1))), WasmInstrI64And])
        ) (enumFromTo 1 62) ++ -- x % (2^bits) = x & (2^bits - 1)
    [
        exactRule ([WasmInstrConst (WasmValueI64 1), WasmInstrI64Div_s], []),
        exactRule ([WasmInstrConst (WasmValueF64 1), WasmInstrF64Div], []),
        exactRule ([WasmInstrConst (WasmValueF64 1), WasmInstrF64Mul], []),
        exactRule ([WasmInstrConst (WasmValueF64 0), WasmInstrF64Mul], [WasmInstrDrop, WasmInstrConst (WasmValueF64 0)]),
        exactRule ([WasmInstrConst (WasmValueI64 1), WasmInstrI64Mul], []),
        exactRule ([WasmInstrConst (WasmValueI64 0), WasmInstrI64Mul], [WasmInstrDrop, WasmInstrConst (WasmValueI64 0)]),
        exactRule ([WasmInstrConst (WasmValueI64 0), WasmInstrI64Add], []),
        exactRule ([WasmInstrConst (WasmValueI64 0), WasmInstrI64Sub], []),
        exactRule ([WasmInstrConst (WasmValueF64 0), WasmInstrF64Add], []),
        exactRule ([WasmInstrConst (WasmValueF64 0), WasmInstrF64Sub], []),
        exactRule ([WasmInstrConst (WasmValueI64 1), WasmInstrI64Rem_s], [WasmInstrDrop, WasmInstrConst (WasmValueI64 0)]),
        exactRule ([WasmInstrI64Eq, WasmInstrI32Eqz], [WasmInstrI64Neq]),
        exactRule ([WasmInstrI64Neq, WasmInstrI32Eqz], [WasmInstrI64Eq]),
        exactRule ([WasmInstrConst (WasmValueI64 0), WasmInstrI64Eq], [WasmInstrI64Eqz]),
        exactRule ([WasmInstrI64Le_s, WasmInstrI32Eqz], [WasmInstrI64Gt_s]),
        exactRule ([WasmInstrI64Lt_s, WasmInstrI32Eqz], [WasmInstrI64Ge_s]),
        exactRule ([WasmInstrI64Ge_s, WasmInstrI32Eqz], [WasmInstrI64Lt_s]),
        exactRule ([WasmInstrI64Gt_s, WasmInstrI32Eqz], [WasmInstrI64Le_s]),
        constDropRule,
        teeRule,
        mulAddRule
    ]

find_pattern_head : (len : Nat) -> (pattern : RewriteRule) -> (xs : List WasmInstr) -> Maybe (Int, Int, List WasmInstr)
find_pattern_head Z pattern xs = case pattern [] of
    Nothing => Nothing
    (Just new) => Just (0, 0, new)
find_pattern_head (S k) pattern xs =
    let h = take (S k) xs in
    case pattern h of
        Nothing => case find_pattern_head k pattern xs of
            Nothing  => Nothing
            (Just r) => Just r
        (Just new) => Just (0, toIntNat (S k), new)

find_pattern : (pattern : RewriteRule) -> (xs : List WasmInstr) -> Maybe (Int, Int, List WasmInstr)
find_pattern pattern [] = case pattern [] of
    Nothing => Nothing
    (Just new) => Just (0, 0, new)
find_pattern pattern (x :: xs) = case find_pattern_head (length (x :: xs)) pattern (x :: xs) of
    Nothing => case find_pattern pattern xs of
        Nothing => Nothing
        (Just (start, len, new)) => Just (start + 1, len, new)
    (Just r) => Just r


-- find_pattern [] xs = Just (0, 0)
-- find_pattern (p :: ps) [] = Nothing
-- find_pattern (p :: ps) (x :: xs) =
--     if p == x
--         then case find_pattern_head ps xs of
--             Nothing => do
--                 (start, len) <- find_pattern (p :: ps) xs
--                 Just (start + 1, len)
--             (Just ()) => Just (0, 1 + toIntNat (length ps))
--         else do
--             (start, len) <- find_pattern (p :: ps) xs
--             Just (start + 1, len)



match_any_pattern : List RewriteRule -> List WasmInstr -> Maybe (Int, Int, List WasmInstr)
match_any_pattern [] xs = Nothing
match_any_pattern (r :: rs) xs =
    case find_pattern r xs of
        Nothing => match_any_pattern rs xs
        (Just res) => Just res

optimize_root_instrs : List WasmInstr -> List WasmInstr
optimize_root_instrs instrs = case match_any_pattern Rules instrs of
    Nothing => instrs
    (Just (start, len, new)) => optimize_root_instrs (replace_range instrs start len new)


optimize_instrs : List WasmInstr -> List WasmInstr
optimize_instrs [] = []
optimize_instrs ((WasmInstrBlock t xs) :: ys) =
    let xs' = optimize_instrs xs in
    let ys' = optimize_instrs ys in
    optimize_root_instrs $ (WasmInstrBlock t xs') :: ys'
optimize_instrs ((WasmInstrIf t xs ys) :: zs) =
    let xs' = optimize_instrs xs in
    let ys' = optimize_instrs ys in
    let zs' = optimize_instrs zs in
    optimize_root_instrs $ (WasmInstrIf t xs' ys') :: zs'
optimize_instrs ((WasmInstrLoop t xs) :: ys) =
    let xs' = optimize_instrs xs in
    let ys' = optimize_instrs ys in
    optimize_root_instrs $ (WasmInstrLoop t xs') :: ys'
optimize_instrs (x :: xs) =
    let xs' = optimize_instrs xs in
    optimize_root_instrs (x :: xs')

optimize_function : WasmFunction -> WasmFunction
optimize_function (MkWasmFunction paramTypes resultType localTypes body id) = MkWasmFunction paramTypes resultType localTypes (optimize_instrs body) id

export
optimize_module : WasmModule -> WasmModule
optimize_module (MkWasmModule funcs start start_type) = MkWasmModule (map optimize_function funcs) start start_type
