module Utils

import Data.Vect
import Data.Fin

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
    mapExcept : (a -> Either e b) -> (input : List a) -> Either e (out : List b ** length out = length input)
    mapExcept f [] = Right ([] ** Refl)
    mapExcept f (x :: xs) = do
        x' <- f x
        (xs' ** prf') <- mapExcept f xs
        pure (x' :: xs' ** cong prf')

namespace vect
    export
    mapExcept : (a -> Either e b) -> Vect n a -> Either e (Vect n b)
    mapExcept f [] = Right []
    mapExcept f (x :: xs) = do
        x' <- f x
        xs' <- mapExcept f xs
        pure (x' :: xs')

namespace list
    export
    map_enum : Int -> (Int -> a -> b) -> List a -> List b
    map_enum acc f [] = []
    map_enum acc f (x :: xs) = f acc x :: map_enum (acc + 1) f xs



namespace vect
    export
    map_enum : (Fin n -> a -> b) -> Vect n a -> Vect n b
    map_enum f xs = map (uncurry f) (zip range xs)


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
