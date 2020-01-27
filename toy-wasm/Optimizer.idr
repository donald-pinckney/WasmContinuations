module Optimizer

import WasmAST

%default covering

data MatchToken a = ExactValue a | Wildcard

-- RewriteRule : Type
-- RewriteRule = (List (WasmInstr' (MatchToken WasmValue) (MatchToken Int) (MatchToken Int) (MatchToken Int) (MatchToken WasmType)), List WasmInstr)

RewriteRule : Type
RewriteRule = List WasmInstr -> Maybe (List WasmInstr)



exactRule : (List WasmInstr, List WasmInstr) -> RewriteRule
exactRule (pat, new) = \instrs => if pat == instrs then Just new else Nothing

Rules : List RewriteRule
Rules =
    [exactRule ([WasmInstrConst (WasmValueI64 1), WasmInstrI64Div_s], [])] ++
    map (\bits => exactRule ([WasmInstrConst (WasmValueI64 (shiftL 1 bits)), WasmInstrI64Div_s],
                   [WasmInstrConst (WasmValueI64 bits), WasmInstrI64Shr_u])
        ) (enumFromTo 1 62) ++
    [exactRule ([WasmInstrConst (WasmValueI64 1), WasmInstrI64Rem_s], [WasmInstrDrop, WasmInstrConst (WasmValueI64 0)])] ++
    map (\bits => exactRule ([WasmInstrConst (WasmValueI64 (shiftL 1 bits)), WasmInstrI64Rem_s],
               [WasmInstrConst (WasmValueI64 (shiftL 1 (bits - 1))), WasmInstrI64And])
    ) (enumFromTo 1 62) ++
    [exactRule ([WasmInstrI64Eq, WasmInstrI32Eqz], [WasmInstrI64Neq])] ++
    [exactRule ([WasmInstrI64Neq, WasmInstrI32Eqz], [WasmInstrI64Eq])] ++
    [exactRule ([WasmInstrConst (WasmValueI64 0), WasmInstrI64Eq], [WasmInstrI64Eqz])] ++
    [exactRule ([WasmInstrI64Le_s, WasmInstrI32Eqz], [WasmInstrI64Gt_s])] ++
    [exactRule ([WasmInstrI64Lt_s, WasmInstrI32Eqz], [WasmInstrI64Ge_s])] ++
    [exactRule ([WasmInstrI64Ge_s, WasmInstrI32Eqz], [WasmInstrI64Lt_s])] ++
    [exactRule ([WasmInstrI64Gt_s, WasmInstrI32Eqz], [WasmInstrI64Le_s])]

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

replace_range : (xs : List a) -> (start : Int) -> (len : Int) -> (new : List a) -> List a
replace_range xs start len new =
    let before = take (cast start {to=Nat}) xs in
    let after = drop (cast (start + len) {to=Nat}) xs in
    before ++ new ++ after

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
optimize_module : Bool -> WasmModule -> WasmModule
optimize_module False m = m
optimize_module True (MkWasmModule funcs start start_type) = MkWasmModule (map optimize_function funcs) start start_type
