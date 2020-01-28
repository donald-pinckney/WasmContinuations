module Result

public export
Result : Type -> Type
Result a = Either String a

export
error : String -> Result a
error message = Left message

export
success : a -> Result a
success val = Right val

-- export
-- monadCompose : Monad m => (a -> m b) -> (b -> c) -> (a -> m c)
-- monadCompose f g x = (f x) >>= g
