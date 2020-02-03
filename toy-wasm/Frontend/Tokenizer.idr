module Tokenizer

import Frontend.TDOP

import Data.String

%default covering

public export
data MyToken = TokVar String
    | TokInt Int
    | TokBool Bool
    | TokFloat Double
    | TokAdd | TokMul | TokSub | TokDiv | TokMod | TokLP | TokRP | TokLB | TokRB | TokComma
    | TokAnd | TokOr | TokNot | TokGT | TokGTE | TokEqEq | TokLTE | TokLT | TokSemi
    | TokIf | TokThen | TokElse | TokWhile | TokEnd | TokFunc | TokTypeInt | TokTypeFloat | TokTypeBool | TokTypeUnit | TokLet | TokSet | TokEq
    | TokDo | TokTo

export
implementation Eq MyToken where
    (TokInt x) == (TokInt y) = x == y
    (TokBool x) == (TokBool y) = x == y
    (TokFloat x) == (TokFloat y) = x == y
    (TokVar x) == (TokVar y) = x == y
    TokAdd == TokAdd = True
    TokMul == TokMul = True
    TokSub == TokSub = True
    TokDiv == TokDiv = True
    TokMod == TokMod = True
    TokLP == TokLP = True
    TokRP == TokRP = True
    TokLB == TokLB = True
    TokRB == TokRB = True
    TokComma == TokComma = True
    TokAnd == TokAnd = True
    TokOr == TokOr = True
    TokNot == TokNot = True
    TokGT == TokGT = True
    TokGTE == TokGTE = True
    TokEqEq == TokEqEq = True
    TokLTE == TokLTE = True
    TokLT == TokLT = True

    TokSemi == TokSemi = True

    TokIf == TokIf = True
    TokThen == TokThen = True
    TokElse == TokElse = True
    TokWhile == TokWhile = True
    TokEnd == TokEnd = True
    TokFunc == TokFunc = True

    TokTypeInt == TokTypeInt = True
    TokTypeFloat == TokTypeFloat = True
    TokTypeBool == TokTypeBool = True
    TokTypeUnit == TokTypeUnit = True

    TokLet == TokLet = True
    TokSet == TokSet = True
    TokEq == TokEq = True

    TokDo == TokDo = True

    TokTo == TokTo = True

    _ == _ = False

%default covering

export
Show MyToken where
    show (TokVar x) = show x
    show (TokInt x) = show x
    show (TokBool x) = show x
    show (TokFloat x) = show x
    show TokAdd = "+"
    show TokMul = "*"
    show TokSub = "-"
    show TokDiv = "/"
    show TokMod = "%"
    show TokLP = "("
    show TokRP = ")"
    show TokIf = "if"
    show TokThen = "then"
    show TokElse = "else"
    show TokWhile = "while"
    show TokEnd = "end"
    show TokFunc = "func"
    show TokLB = "{"
    show TokRB = "}"
    show TokComma = ","
    show TokTypeInt = "int"
    show TokTypeFloat = "float"
    show TokTypeBool = "bool"
    show TokTypeUnit = "unit"
    show TokAnd = "&&"
    show TokOr = "||"
    show TokNot = "!"
    show TokGT = ">"
    show TokGTE = ">="
    show TokEqEq = "=="
    show TokLTE = "<="
    show TokLT = "<"
    show TokEq = "="
    show TokLet = "let"
    show TokSet = "set"
    show TokSemi = ";"
    show TokDo = "do"
    show TokTo = "to"



isVarChar : Char -> Bool
isVarChar x = isAlpha x || isDigit x || x == '.'

isWhitespace : Char -> Bool
isWhitespace ' ' = True
isWhitespace '\n' = True
isWhitespace '\r' = True
isWhitespace '\t' = True
isWhitespace _ = False


charHandler : TokenHandler Nat Char (List (Either String MyToken))
charHandler '+' = charToken (Right TokAdd)
charHandler '-' = charToken (Right TokSub)
charHandler '*' = charToken (Right TokMul)
charHandler '/' = charToken (Right TokDiv)
charHandler '%' = charToken (Right TokMod)
charHandler '(' = charToken (Right TokLP)
charHandler ')' = charToken (Right TokRP)
charHandler '{' = charToken (Right TokLB)
charHandler '}' = charToken (Right TokRB)
charHandler ',' = charToken (Right TokComma)
charHandler ';' = charToken (Right TokSemi)
charHandler '!' = charToken (Right TokNot)

charHandler '<' = strToken "=" (Right TokLTE) (Right TokLT)
charHandler '>' = strToken "=" (Right TokGTE) (Right TokGT)
charHandler '=' = strToken "=" (Right TokEqEq) (Right TokEq)

charHandler '&' = strToken "&" (Right TokAnd) (Left "&")
charHandler '|' = strToken "|" (Right TokAnd) (Left "|")

charHandler c =
    if isWhitespace c then
        MkTokenAction
            (Just $ \r_s,p => if length r_s == 0 then Right ([], r_s) else [(r, rest) | (r, rest) <- p (precedence 10) r_s])
            (Just $ \l,r_s,p => if length r_s == 0 then Right (l, r_s) else [(l ++ r, rest) | (r, rest) <- p (precedence 10) r_s])
            (Just 10)
    else if isVarChar c then
        MkTokenAction
            (Just $ \r_s,p =>
                if length r_s == 0 || isJust (fst (headMatches (not . isVarChar) r_s)) then pure ([Left (singleton c)], r_s) else do
                ([Left str], rest) <- p (precedence 100) r_s
                    | ([], rest) => Left "bad1" --pure ([singleton c], rest)
                    | (el, rest) => Left "bad2"
                pure ([Left (strCons c str)], rest)
            )
            Nothing
            Nothing
    else MkTokenAction Nothing Nothing Nothing


postProcessRawToken : Either String MyToken -> MyToken
postProcessRawToken (Left "if") = TokIf
postProcessRawToken (Left "true") = TokBool True
postProcessRawToken (Left "false") = TokBool False
postProcessRawToken (Left "then") = TokThen
postProcessRawToken (Left "else") = TokElse
postProcessRawToken (Left "while") = TokWhile
postProcessRawToken (Left "do") = TokDo
postProcessRawToken (Left "end") = TokEnd
postProcessRawToken (Left "func") = TokFunc
postProcessRawToken (Left "int") = TokTypeInt
postProcessRawToken (Left "float") = TokTypeFloat
postProcessRawToken (Left "bool") = TokTypeBool
postProcessRawToken (Left "let") = TokLet
postProcessRawToken (Left "set") = TokSet
postProcessRawToken (Left "to") = TokTo
postProcessRawToken (Left "unit") = TokTypeUnit

postProcessRawToken (Left str) =
    case parseInteger {a=Int} str of
        (Just n) => TokInt n
        Nothing => case parseDouble str of
            (Just f) => TokFloat f
            Nothing => TokVar str

postProcessRawToken (Right t) = t


export
tokenize : String -> Either String (List MyToken)
tokenize str = do
    tmp_toks <- parse charHandler (unpack str)
    pure $ map postProcessRawToken tmp_toks
