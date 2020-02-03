module TDOP

import Data.String

%default covering

export
record TokenAction pt tt rt where
    constructor MkTokenAction
    null_handler : Maybe ((right : List tt) -> (parse : (pt, Nat) -> List tt -> Either String (rt, List tt)) -> Either String (rt, List tt))
    left_handler : Maybe ((left : rt) -> (right : List tt) -> (parse : (pt, Nat) -> List tt -> Either String (rt, List tt)) -> Either String (rt, List tt))
    lbp : Maybe pt

public export
TokenHandler : Type -> Type -> Type -> Type
TokenHandler pt tt rt = tt -> TokenAction pt tt rt

maybeMax : Ord pt => Maybe pt -> Maybe pt -> Maybe pt
maybeMax Nothing Nothing = Nothing
maybeMax Nothing (Just y) = Just y
maybeMax (Just x) Nothing = Just x
maybeMax (Just x) (Just y) = Just (max x y)

export
implementation Ord pt => Semigroup (TokenAction pt tt rt) where
    (MkTokenAction a1 a2 a3) <+> (MkTokenAction b1 b2 b3) = MkTokenAction (a1 <+> b1) (a2 <+> b2) (maybeMax a3 b3)

export
precedence : pt -> (pt, Nat)
precedence prec = (prec, 1)

export
lower_precedence : pt -> (pt, Nat)
lower_precedence prec = (prec, 0)


mutual
    expression_left : (MinBound pt, Show tt, Show rt) => TokenHandler pt tt rt -> (left : rt) -> (rbp : (pt, Nat)) -> List tt -> Either String (rt, List tt)
    expression_left handler left rbp [] = Right (left, [])
    expression_left handler left rbp str@(token :: rest) =
        let (MkTokenAction null_handler left_handler lbp) = handler token in
        case lbp of
            Nothing => Left ("Unknown operator: " ++ show token)
            (Just the_lbp) =>
                if rbp < precedence the_lbp then
                    case left_handler of
                        Nothing => Left "expected infix operator"
                        (Just left_handler') => do
                            (left', rest') <- left_handler' left rest (expression handler)
                            expression_left handler left' rbp rest'
                else
                    Right (left, str)

    export
    expression : (MinBound pt, Show tt, Show rt) => TokenHandler pt tt rt -> (rbp : (pt, Nat)) -> List tt -> Either String (rt, List tt)
    expression handler rbp [] = Left "Expected input to parse (1)"
    expression handler rbp str@[x] =
        let (MkTokenAction null_handler left_handler lbp) = handler x in
        case null_handler of
            Nothing => Left ("expected prefix operator (1): " ++ show x)
            (Just null_handler') => null_handler' [] (expression handler)
    expression handler rbp str@(t :: token :: rest) =
        let (MkTokenAction null_handler left_handler lbp) = handler t in
        case null_handler of
            Nothing => Left ("expected prefix operator (2): " ++ show t)
            (Just null_handler') => do
                (left, rest') <- null_handler' (token :: rest) (expression handler)
                expression_left handler left rbp rest'

export
parse : (MinBound pt, Show tt, Show rt) => TokenHandler pt tt rt -> List tt -> Either String rt
parse handler program = do
    (res, rest) <- expression handler (precedence (minBound)) program
    if length rest == 0
        then Right res
        else Left $ "Error: Not fully parsed: " ++ show rest


export
match : (Eq tt, Show tt) => tt -> List tt -> Either String (List tt)
match x [] = Left $ "expected " ++ show x
match x (y :: ys) = if x == y then Right (ys) else Left $ "expected " ++ show x

export
handle_infixl : pt -> (rt -> rt -> rt) -> TokenAction pt tt rt
handle_infixl prec op = MkTokenAction Nothing (Just $ \l,r_s,p => [(op l r, rest) | (r, rest) <- p (precedence prec) r_s]) (Just prec)

export
handle_infixr : pt -> (rt -> rt -> rt) -> TokenAction pt tt rt
handle_infixr prec op = MkTokenAction Nothing (Just $ \l,r_s,p => [(op l r, rest) | (r, rest) <- p (lower_precedence prec) r_s]) (Just prec)

export
handle_prefix : pt -> (rt -> rt) -> TokenAction pt tt rt
handle_prefix prec op = MkTokenAction (Just $ \r_s,p => [(op r, rest) | (r, rest) <- p (precedence prec) r_s]) Nothing Nothing

export
handle_group_left : (MinBound pt, Eq tt, Show tt) => (right_group : tt) -> TokenAction pt tt rt
handle_group_left right_group = MkTokenAction (Just $ \r_s,p => do
        (e, rest) <- p (precedence (minBound)) r_s
        let (Right rest) = match right_group rest
            | Left err => Left err
        pure (e, rest)
    ) Nothing Nothing

export
handle_group_right : MinBound pt => TokenAction pt tt rt
handle_group_right = MkTokenAction Nothing Nothing (Just minBound)

export
headMatches : (a -> Bool) -> List a -> (Maybe a, List a)
headMatches f [] = (Nothing, [])
headMatches f (x :: xs) = if f x then (Just x, xs) else (Nothing, x :: xs)

export
testHead : (a -> Bool) -> List a -> Bool
testHead f [] = False
testHead f (x :: xs) = f x

export
matchStart : Eq a => List a -> List a -> (Bool, List a)
matchStart [] xs = (True, xs)
matchStart (p :: ps) [] = (False, [])
matchStart (p :: ps) (x :: xs) = if p == x then matchStart ps xs else (False, x :: xs)

export
charToken : tt -> TokenAction Nat Char (List tt)
charToken tok = handle_infixl 10 (\l,r => l ++ (tok :: r)) <+> handle_prefix 10 (\r => tok :: r)

export
strToken : String -> tt -> tt -> TokenAction Nat Char (List tt)
strToken after if_tok else_tok =
    MkTokenAction
        (Just $ \r_s,p =>
            let (has_eq, r_s) = matchStart (unpack after) r_s in
            if has_eq
                then [(if_tok :: r, rest) | (r, rest) <- p (precedence 10) r_s]
                else [(else_tok :: r, rest) | (r, rest) <- p (precedence 10) r_s]
        )
        (Just $ \l,r_s,p =>
            let (has_eq, r_s) = matchStart (unpack after) r_s in
            if has_eq
                then [(l ++ if_tok :: r, rest) | (r, rest) <- p (precedence 10) r_s]
                else [(l ++ else_tok :: r, rest) | (r, rest) <- p (precedence 10) r_s]
        )
        (Just 10)

export
parse_list : (MinBound pt, Eq tt) => (parse : (pt, Nat) -> List tt -> Either String (rt, List tt)) -> tt -> List tt -> Either String (List rt, List tt)
parse_list p sep rest = do
    (next, rest) <- p (precedence minBound) rest
    let (is_comma, rest) = matchStart [sep] rest
    if is_comma
        then do
            (others, rest) <- parse_list p sep rest
            pure (next :: others, rest)
        else pure ([next], rest)
