module Misc.ArgParse

import public Control.Monad.State

matchFlag' : String -> List String -> (Bool, List String)
matchFlag' y [] = (False, [])
matchFlag' y (x :: xs) =
    if y == x then
        (True, xs)
    else
        let (out, args') = matchFlag' y xs in
        (out, x :: args')

export
matchFlag : String -> StateT (List String) (Either String) Bool
matchFlag y = ST (Right . (matchFlag' y))

matchOption' : String -> List String -> (Maybe String, List String)
matchOption' y [] = (Nothing, [])
matchOption' y (x :: []) = (Nothing, [x])
matchOption' y (x1 :: (x2 :: xs)) =
    if x1 == y then
        (Just x2, xs)
    else
        let (out, args') = matchOption' y (x2 :: xs) in
        (out, x1 :: args')

export
matchOption : String -> StateT (List String) (Either String) (Maybe String)
matchOption y = ST (Right . (matchOption' y))

export
matchDefaultOption : String -> String -> StateT (List String) (Either String) String
matchDefaultOption y d = do
    Just r <- matchOption y
        | Nothing => pure d
    pure r

assertOnlyArgument' : List String -> Either String (String, List String)
assertOnlyArgument' [x] = Right (x, [])
assertOnlyArgument' otherwise = Left $ "Unrecognized arguments: "

export
assertOnlyArgument : StateT (List String) (Either String) String
assertOnlyArgument = ST assertOnlyArgument'

export
parseArgs : StateT (List String) (Either String) a -> IO (Either String a)
parseArgs f = do
    (prog :: args) <- getArgs
    let Right (x, s) = runStateT f args
        | Left err => pure (Left err)
    pure (Right x)
