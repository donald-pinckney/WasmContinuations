module Utils

import Data.Vect

%default covering

namespace int
    export
    replace_range : (xs : List a) -> (start : Int) -> (len : Int) -> (new : List a) -> List a
    replace_range xs start len new =
        let before = take (cast start {to=Nat}) xs in
        let after = drop (cast (start + len) {to=Nat}) xs in
        before ++ new ++ after

namespace nat
    export
    replace_range : (xs : List a) -> (start : Nat) -> (len : Nat) -> (new : List a) -> List a
    replace_range xs start len new =
        let before = take start xs in
        let after = drop (start + len) xs in
        before ++ new ++ after

namespace list
    export
    mapExcept : (a -> Either e b) -> List a -> Either e (List b)
    mapExcept f [] = Right []
    mapExcept f (x :: xs) = do
        x' <- f x
        xs' <- mapExcept f xs
        pure (x' :: xs')

namespace vect
    export
    mapExcept : (a -> Either e b) -> Vect n a -> Either e (Vect n b)
    mapExcept f [] = Right []
    mapExcept f (x :: xs) = do
        x' <- f x
        xs' <- mapExcept f xs
        pure (x' :: xs')

export
allUnique : Eq a => List a -> Bool
allUnique xs = union [] xs == xs


public export
Result : Type -> Type
Result a = Either String a

export
error : String -> Result a
error message = Left message

export
success : a -> Result a
success val = Right val
