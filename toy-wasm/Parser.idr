module Parser

import ParseUtils
import Result
-- import Shared.TestingSupport
-- import CST
import Identifier
import ToyAST


ParseResultInternal : Type -> Type
ParseResultInternal t = Result (t, SourceString)

isVarChar : Char -> Bool
isVarChar c = isAlpha c

isVarCharStart : Char -> Bool
isVarCharStart = isVarChar

isStartOfTerm : Char -> Bool
isStartOfTerm c = isVarCharStart c || c == '(' || c == '\\' || c == '*' || c == '[' || c == '?' || c == '{'

parseIdentifier : SourceString -> SourceString -> ParseResultInternal Identifier
-- parseIdentifier acc [] = success (MkIdentifier acc False, [])
-- parseIdentifier acc vStr@((nx, cx) :: xs) =
--     if isVarChar cx then
--         parseIdentifier (acc ++ [(nx, cx)]) xs
--     else if isWhitespace cx then
--         success (MkIdentifier acc False, xs)
--     else if length acc == 0 then
--         error "Expected variable to parse"
--     else
--         success (MkIdentifier acc False, vStr)



-- groupApps : CExpr -> List CExpr -> CExpr
-- groupApps acc [] = acc
-- groupApps acc (t :: ts) = groupApps (CExprApp acc t) ts


mutual

    -- *xe:a, y : b, zr:c.   ->   xe:a, *y : b, zr:c.
    -- xe:a, y : b, *zr:c.   ->   xe:a, y : b, zr:c*.
    -- parseVarAndType : SourceString -> ParseResultInternal CDecl
    -- parseVarAndType xs = do
    --     (v, xs) <- parseIdentifier [] xs
    --     let xs = eatWhitespace xs
    --     xs <- expect xs ':'
    --     let xs = eatWhitespace xs
    --     (t, xs) <- parseTerm xs
    --     let xs = eatWhitespace xs
    --     let xs = eatOneChar xs ','
    --     let xs = eatWhitespace xs
    --     pure (MkCDecl v t, xs)


    -- *x, y, z.   ->   x, y, z.*
    -- parseLambdaVars : SourceString -> ParseResultInternal (List CDecl)
    -- parseLambdaVars ((nx, '.') :: xs) = success ([], xs)
    -- parseLambdaVars varsStr = do
    --     (varAndType, rest) <- parseVarAndType (eatWhitespace varsStr)
    --     (moreVarsAndTypes, rest2) <- parseLambdaVars rest
    --     pure (varAndType :: moreVarsAndTypes, rest2)



    -- Something like \x y z.M
    -- But this starts with '\' already removed.
    -- parseLambda : SourceString -> ParseResultInternal CExpr
    -- parseLambda str = do
    --     (varsAndTypes, bodyStr) <- parseLambdaVars str
    --     (body, rest) <- parseTerm bodyStr
    --     pure (CExprLambda varsAndTypes body, rest)


    parseCommaTermList : SourceString -> ParseResultInternal (List (Expr cd fns))
    -- parseCommaTermList str = do
    --     let str = eatWhitespace str
    --     if not (any (isStartOfTerm . snd) (head' str)) then success ([], str) else do
    --         (e, str) <- parseTerm str
    --         let str = eatWhitespace str
    --         case eatAndMatch str "," of
    --             (str, True) => do
    --                 (es, str) <- parseCommaTermList str
    --                 success (e :: es, str)
    --             (str, False) => success ([e], str)



    parseTerm : SourceString -> ParseResultInternal (Expr cd fns)
    -- parseTerm w_str = do
    --     let str = eatWhitespace w_str
    --     (tList, rest) <- parseTermList str
    --     case tList of
    --         t :: ts => success (groupApps t ts, rest)
    --         [] => error "Expected to parse term"







mutual
    -- parseLine : SourceString -> ParseResultInternal CLine
    -- parseLine str = do
    --     str <- expect str '('
    --     let str = eatWhitespace str
    --     case eatAndMatch str "Suppose " of
    --         (str, True) => do
    --             let str = eatWhitespace str
    --             (x, str) <- parseIdentifier [] str
    --             let str = eatWhitespace str
    --             (t, str) <- parseExpr_sexp str
    --             let str = eatWhitespace str
    --             (b, str) <- parseBook str
    --             let str = eatWhitespace str
    --             str <- expect str ')'
    --             success $ (CLineSuppose (MkCDecl x t) b, str)
    --         (str, False) =>
    --             case eatAndMatch str "Def " of
    --                 (str, False) => error "Expected either 'Suppose' or 'Def'"
    --                 (str, True) => do
    --                     let str = eatWhitespace str
    --                     (x, str) <- parseIdentifier [] str
    --                     let str = eatWhitespace str
    --                     str <- expect str '('
    --                     (params, str) <- parseIdentifierList str
    --                     let str = eatWhitespace str
    --                     str <- expect str ')'
    --                     let str = eatWhitespace str
    --                     (e, str) <- parseExpr_sexp str
    --                     let str = eatWhitespace str
    --                     (t, str) <- parseExpr_sexp str
    --                     let str = eatWhitespace str
    --                     str <- expect str ')'
    --                     success (CLineDef (MkCDef x params e t), str)

    parseModule : SourceString -> ParseResultInternal (Module nmfns)
    -- parseBook [] = success $ ([], [])
    -- parseBook str@((nx, ')') :: rest) = success $ ([], str)
    -- parseBook str = do
    --     let str2 = eatWhitespace str
    --     (line, str3) <- parseLine str2
    --     let str4 = eatWhitespace str3
    --     (rest, str5) <- parseBook str4
    --     success $ (line :: rest, str5)

export
parse_unpacked : SourceString -> Result (Module nmfns)
parse_unpacked {nmfns} str = do
    let Right (parsed, []) = parseModule {nmfns=nmfns} $ str
        | Left err => Left err
        | Right (parsed, remainingStr) =>
            Left ("Remaining input not parsed: " ++ packSource remainingStr)
    success parsed

export
parse : String -> Result (Module nmfns)
parse str = parse_unpacked (unpackSource str)
