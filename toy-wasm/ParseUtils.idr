module ParseUtils

import Data.Vect
import Result

public export
SourceString : Type
SourceString = List (Nat, Char)

export
unpackSource : String -> SourceString
unpackSource str =
    let chars = unpack str in
    zip (take (length chars) [0..]) chars

export
toChars : SourceString -> List Char
toChars xs = map snd xs

export
packSource : SourceString -> String
packSource = pack . toChars


export
isWhitespace : Char -> Bool
isWhitespace c = c == ' ' || c == '\n' || c == '\t' || c == '\r'

export
eatWhitespace : SourceString -> SourceString
eatWhitespace [] = []
eatWhitespace str@((n, c) :: xs) =
    if isWhitespace c then
        eatWhitespace xs
    else
        str

export
eatOneChar : SourceString -> Char -> SourceString
eatOneChar [] c = []
eatOneChar str@((n, c') :: xs) c = if c' == c then xs else str


eatAndMatch_impl : SourceString -> List Char -> (SourceString, Bool)
eatAndMatch_impl [] [] = ([], True)
eatAndMatch_impl (x :: xs) [] = (x :: xs, True)
eatAndMatch_impl [] (y :: ys) = ([], False)
eatAndMatch_impl ((nx, cx) :: xs) (y :: ys) =
    if cx == y then
        let (rest, didMatch) = eatAndMatch_impl xs ys in
        if didMatch then
            (rest, True)
        else
            ((nx, cx) :: xs, False)
    else
        ((nx, cx) :: xs, False)

export
eatAndMatch : SourceString -> String -> (SourceString, Bool)
eatAndMatch xs x = eatAndMatch_impl xs (unpack x)

export
expect : SourceString -> Char -> Result SourceString
expect [] c = error $ "Expected '" ++ (singleton c) ++ "', but no input left."
expect ((nx, cx) :: xs) c =
    if cx == c then
        success xs
    else
        error $ "Expected '" ++ (singleton c) ++ "', got '" ++ (singleton cx) ++ "'"
