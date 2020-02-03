module Parser

import Frontend.TDOP
import Frontend.Tokenizer

import LangDefs.ToyAST

import Utils

public export
data PExpr =
    PExprVar String | PExprValue Value
    | PExprDeclareVar Type' String PExpr PExpr
    | PExprUpdateVar String PExpr PExpr
    | PExprCall String (List PExpr)
    | PExprIf PExpr PExpr PExpr
    | PExprWhile PExpr PExpr PExpr
    | PExprAdd PExpr PExpr
    | PExprSub PExpr PExpr
    | PExprNeg PExpr
    | PExprMul PExpr PExpr
    | PExprDiv PExpr PExpr
    | PExprMod PExpr PExpr
    | PExprGT PExpr PExpr
    | PExprGTE PExpr PExpr
    | PExprEQ PExpr PExpr
    | PExprLTE PExpr PExpr
    | PExprLT PExpr PExpr
    | PExprAnd PExpr PExpr
    | PExprOr PExpr PExpr
    | PExprNot PExpr

export
Show PExpr where
    show (PExprVar x) = x
    show (PExprValue x) = show x
    show (PExprDeclareVar x s y z) = "let " ++ show x ++ " " ++ s ++ " = (" ++ show y ++ ");(\n" ++ show z ++ ")"
    show (PExprUpdateVar x y z) = "set " ++ x ++ " = (" ++ show y ++ ");(\n" ++ show z ++ ")"
    show (PExprCall x xs) = x ++ "[" ++ show xs ++ "]"
    show (PExprIf x y z) = "if (" ++ show x ++ ") then (" ++ show y ++ ") else (" ++ show z ++ ")"
    show (PExprWhile x y z) = "while (" ++ show x ++ ")(\n" ++ show y ++ ")\nend\n(" ++ show z ++ ")"
    show (PExprAdd x y) = "(" ++ show x ++ ") + (" ++ show y ++ ")"
    show (PExprSub x y) = "(" ++ show x ++ ") - (" ++ show y ++ ")"
    show (PExprNeg x) = "-(" ++ show x ++ ")"
    show (PExprMul x y) = "(" ++ show x ++ ") * (" ++ show y ++ ")"
    show (PExprDiv x y) = "(" ++ show x ++ ") / (" ++ show y ++ ")"
    show (PExprMod x y) = "(" ++ show x ++ ") % (" ++ show y ++ ")"
    show (PExprGT x y) = "(" ++ show x ++ ") > (" ++ show y ++ ")"
    show (PExprGTE x y) = "(" ++ show x ++ ") >= (" ++ show y ++ ")"
    show (PExprEQ x y) = "(" ++ show x ++ ") == (" ++ show y ++ ")"
    show (PExprLTE x y) = "(" ++ show x ++ ") <= (" ++ show y ++ ")"
    show (PExprLT x y) = "(" ++ show x ++ ") < (" ++ show y ++ ")"
    show (PExprAnd x y) = "(" ++ show x ++ ") && (" ++ show y ++ ")"
    show (PExprOr x y) = "(" ++ show x ++ ") || (" ++ show y ++ ")"
    show (PExprNot x) = "!(" ++ show x ++ ")"

public export
record PFunc where
    constructor MkPFunc
    name : String
    returnType : Type'
    params : List (String, Type')
    body : PExpr

export
Show PFunc where
    show (MkPFunc name returnType params body) = "func " ++ show returnType ++ " " ++ name ++ "(" ++ show params ++ ") {\n" ++ show body ++ "}\n"

isTypeTok : MyToken -> Bool
isTypeTok TokTypeInt = True
isTypeTok TokTypeFloat = True
isTypeTok TokTypeBool = True
isTypeTok _ = False

isVarTok : MyToken -> Bool
isVarTok (TokVar x) = True
isVarTok _ = False

coerceToType : MyToken -> Either String Type'
coerceToType TokTypeBool = Right TypeBool
coerceToType TokTypeInt = Right TypeInt
coerceToType TokTypeFloat = Right TypeDouble
coerceToType _ = Left "expected type"

extractVar : MyToken -> Either String String
extractVar (TokVar x) = Right x
extractVar _ = Left "expected var"

extractVar_expr : PExpr -> Either String String
extractVar_expr (PExprVar x) = Right x
extractVar_expr _ = Left "expected var"




-- = \rest =>
--     do
--         (next, rest) <- p (precedence 0) rest
--         let (is_comma, rest) = matchStart [TokComma] rest
--         if is_comma
--             then do
--                 (others, rest) <- parse_list rest
--                 ?opiuwerwe
--             else pure ([next], rest)

exprHandler : TokenHandler Nat MyToken PExpr
exprHandler (TokInt n) = MkTokenAction (Just $ \r_s,p => Right (PExprValue (ValueInt n), r_s)) Nothing Nothing
exprHandler (TokFloat f) = MkTokenAction (Just $ \r_s,p => Right (PExprValue (ValueFloat f), r_s)) Nothing Nothing
exprHandler (TokBool b) = MkTokenAction (Just $ \r_s,p => Right (PExprValue (ValueBool b), r_s)) Nothing Nothing
exprHandler (TokVar v) = MkTokenAction (Just $ \r_s,p => Right (PExprVar v, r_s)) Nothing Nothing
exprHandler TokAdd = handle_infixl 10 PExprAdd
exprHandler TokSub = handle_prefix 100 PExprNeg <+> handle_infixl 10 PExprSub
exprHandler TokMul = handle_infixl 20 PExprMul
exprHandler TokDiv = handle_infixl 20 PExprDiv
exprHandler TokMod = handle_infixl 20 PExprMod
exprHandler TokGT = handle_infixl 8 PExprGT
exprHandler TokGTE = handle_infixl 8 PExprGTE
exprHandler TokEqEq = handle_infixl 8 PExprEQ
exprHandler TokLTE = handle_infixl 8 PExprLTE
exprHandler TokLT = handle_infixl 8 PExprLT

exprHandler TokAnd = handle_infixl 6 PExprAnd
exprHandler TokOr = handle_infixl 4 PExprOr
exprHandler TokNot = handle_prefix 7 PExprNot

exprHandler TokLP = (handle_group_left TokRP) <+>
    MkTokenAction
        Nothing
        (Just $ \l,r_s,p =>
            do
                f <- extractVar_expr l
                let (is_end, rest) = matchStart [TokRP] r_s
                if is_end
                    then pure (PExprCall f [], rest)
                    else do
                        (args, rest) <- parse_list p TokComma rest
                        rest <- match TokRP rest
                        pure (PExprCall f args, rest)


        )
        (Just 200)

exprHandler TokRP = handle_group_right
exprHandler TokComma = handle_group_right

exprHandler TokLet =
    MkTokenAction
        (Just $ \r_s,p => do
            let (Just tt, rest) = headMatches isTypeTok r_s
                | (Nothing, rest) => Left "expected type in variable declaration"
            t <- coerceToType tt
            let (Just vt, rest) = headMatches isVarTok rest
                | (Nothing, rest) => Left "expected variable name in variable declaration"
            v <- extractVar vt

            rest <- match TokEq rest
            (initExpr, rest) <- p (precedence 0) rest
            rest <- match TokSemi rest
            (after, rest) <- p (precedence 0) rest
            pure (PExprDeclareVar t v initExpr after, rest)
        )
        Nothing
        Nothing
exprHandler TokSet =
    MkTokenAction
        (Just $ \r_s,p => do
            let (Just vt, rest) = headMatches isVarTok r_s
                | (Nothing, rest) => Left "expected variable name in variable declaration"
            v <- extractVar vt

            rest <- match TokEq rest
            (initExpr, rest) <- p (precedence 0) rest
            rest <- match TokSemi rest
            (after, rest) <- p (precedence 0) rest
            pure (PExprUpdateVar v initExpr after, rest)
        )
        Nothing
        Nothing
exprHandler TokEq = handle_group_right
exprHandler TokSemi = handle_group_right

exprHandler TokIf =
    MkTokenAction
        (Just $ \r_s,p => do
            (cond, rest) <- p (precedence 0) r_s
            rest <- match TokThen rest
            (then_b, rest) <- p (precedence 0) rest
            rest <- match TokElse rest
            (else_b, rest) <- p (precedence 0) rest
            pure (PExprIf cond then_b else_b, rest)
        )
        Nothing
        Nothing
exprHandler TokThen = handle_group_right
exprHandler TokElse = handle_group_right

exprHandler TokWhile =
    MkTokenAction
        (Just $ \r_s,p => do
            (cond, rest) <- p (precedence 0) r_s
            let (Right rest) = match TokDo rest
                | Left err => Left err
            (body, rest) <- p (precedence 0) rest
            let (Right rest) = match TokEnd rest
                | Left err => Left err
            (after, rest) <- p (precedence 0) rest
            pure (PExprWhile cond body after, rest)
        )
        Nothing
        Nothing
exprHandler TokDo = handle_group_right
exprHandler TokEnd = handle_group_right
exprHandler TokRB = handle_group_right

export
parseExpr : List MyToken -> Either String PExpr
parseExpr xs = parse exprHandler xs


paramHandler : TokenHandler Nat MyToken (String, Type')
paramHandler TokTypeInt = handle_prefix 1 (\(s, t) => (s, TypeInt))
paramHandler TokTypeFloat = handle_prefix 1 (\(s, t) => (s, TypeDouble))
paramHandler TokTypeBool = handle_prefix 1 (\(s, t) => (s, TypeBool))
paramHandler (TokVar v) = MkTokenAction (Just $ \r_s,p => pure ((v, TypeInt), r_s)) Nothing Nothing -- type int will get replaced
paramHandler TokRP = handle_group_right
paramHandler TokComma = handle_group_right


parseFuncs : List MyToken -> Either String (List PFunc)
parseFuncs [] = Right []
parseFuncs (TokFunc :: rest) = do
    let (Just tt, rest) = headMatches isTypeTok rest
        | (Nothing, rest) => Left "expected type after func"
    t <- coerceToType tt

    let (Just vt, rest) = headMatches isVarTok rest
        | (Nothing, rest) => Left "expected variable name in variable declaration"
    v <- extractVar vt

    rest <- match TokLP rest
    let (is_end, rest) = matchStart [TokRP] rest
    (params, rest) <- if is_end
                    then pure (the (List (String, Type')) [], rest)
                    else do
                            (args, rest) <- parse_list (expression paramHandler) TokComma rest
                            rest <- match TokRP rest
                            pure (args, rest)
    -- params <- mapExcept extractVar_expr paramExprs
    rest <- match TokLB rest
    (body, rest) <- expression exprHandler (precedence 0) rest
    rest <- match TokRB rest

    let func = MkPFunc v t params body
    otherFuncs <- parseFuncs rest
    pure (func :: otherFuncs)

parseFuncs (x :: xs) = Left "expected func keyword"



-- The main function is checked to exist and will be the first function in the list
export
parseModule : List MyToken -> Either String (PFunc, List PFunc)
parseModule xs = do
    fs <- parseFuncs xs
    let names = map name fs
    let True = allUnique names
        | False => Left "Duplicate function names!"
    let [main_idx] = findIndices (\f => name f == "main") fs
        | [] => Left "You must have a 'main' function!"
        | otherwise => Left "unreachable"

    let Just main_f = index' main_idx fs
        | Nothing => Left "unreachable"
    let [] = params main_f
        | otherwise => Left "main function must have 0 parameters"

    pure (main_f, replace_range fs main_idx 1 [])
