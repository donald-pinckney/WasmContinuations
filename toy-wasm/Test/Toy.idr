module Test.Toy

import ToyAST
import ToyInterp
import Data.Vect

assert_interp : Module nmfns -> Value -> IO ()
assert_interp m expected = case interp_module m of
    (Left err) => putStrLn $ "Test failed, error during interpretation: " ++ err
    (Right v) => if v == expected
        then putStrLn "Test passed"
        else putStrLn $ "Test failed, expected: " ++ show expected ++ ", received: " ++ show v

assert_interp_fail : Module nmfns -> IO ()
assert_interp_fail m = case interp_module m of
    (Left err) => putStrLn "Test passed"
    (Right v) => putStrLn $ "Test failed, expected interpretation failure, received result: " ++ show v

export
toy1 : Module 0
toy1 = MkModule [
    MkFuncDef TypeInt [] (
        ExprValue (ValueInt 3)
    )
]

test1 : IO ()
test1 = assert_interp toy1 (ValueInt 3)

export
varsProg : Module 0
varsProg = MkModule [
    MkFuncDef TypeInt [] (
        {-
            let x : Int = 3 + 6;
            let y : Int = x - 5;
            y = y + 1;
            x % y
        -}
        ExprDeclareVar TypeInt (ExprIAdd (ExprValue (ValueInt 3)) (ExprValue (ValueInt 6))) $
        ExprDeclareVar TypeInt (ExprISub (ExprVar 0) (ExprValue (ValueInt 5))) $
        ExprUpdateVar 0 (ExprIAdd (ExprVar 0) (ExprValue (ValueInt 1))) $
        ExprIMod (ExprVar 1) (ExprVar 0)
    )
]

testVars : IO ()
testVars = assert_interp varsProg (ValueInt 4)

export
hailstone_iter : Module 2
hailstone_iter = MkModule [
    MkFuncDef TypeInt [] (
        -- ExprCall 1 [ExprValue (ValueInt 10000000)]
        ExprCall 1 [ExprValue (ValueInt 1000)]
    ),
    MkFuncDef TypeInt [TypeInt] (
        ExprDeclareVar TypeInt (ExprValue (ValueInt 0)) $ -- hailstone count
        ExprDeclareVar TypeInt (ExprValue (ValueInt 1)) $ -- loop variable
        ExprWhile (ExprILTE (ExprVar 0) (ExprVar 2)) ( -- while loop var <= argument
            ExprUpdateVar 1 (ExprIAdd (ExprCall 2 [ExprVar 0]) (ExprVar 1)) $ -- update hailstone count
            ExprUpdateVar 0 (ExprIAdd (ExprVar 0) (ExprValue (ValueInt 1))) $ -- update loop variable
            ExprValue (ValueInt 42) -- this is discarded
        ) $
        ExprVar 1
    ),
    MkFuncDef TypeInt [TypeInt] (
        ExprDeclareVar TypeInt (ExprValue (ValueInt 0)) $ -- hailstone count
        ExprWhile (ExprNot (ExprIEQ (ExprVar 1) (ExprValue (ValueInt 1)))) ( -- while loop var != 1
            ExprUpdateVar 0 (ExprIAdd (ExprVar 0) (ExprValue (ValueInt 1))) $ -- update hailstone count
            ExprIf (ExprIEQ (ExprIMod (ExprVar 1) (ExprValue (ValueInt 2))) (ExprValue (ValueInt 0))) (
                ExprUpdateVar 1 (ExprIDiv (ExprVar 1) (ExprValue (ValueInt 2))) $ -- update hailstone count
                ExprValue (ValueInt 42) -- this is discarded
            ) (
                ExprUpdateVar 1 (ExprIAdd (ExprIMul (ExprValue (ValueInt 3)) (ExprVar 1)) (ExprValue (ValueInt 1))) $ -- update hailstone count
                ExprValue (ValueInt 42) -- this is discarded
            )
        ) $
        ExprVar 0
    )
]

export
hailstone_rec : Module 2
hailstone_rec = MkModule [
    MkFuncDef TypeInt [] (
        -- ExprCall 1 [ExprValue (ValueInt 10000000)]
        ExprCall 1 [ExprValue (ValueInt 1000)]
    ),
    MkFuncDef TypeInt [TypeInt] (
        ExprDeclareVar TypeInt (ExprValue (ValueInt 0)) $ -- hailstone count
        ExprDeclareVar TypeInt (ExprValue (ValueInt 1)) $ -- loop variable
        ExprWhile (ExprILTE (ExprVar 0) (ExprVar 2)) ( -- while loop var <= argument
            ExprUpdateVar 1 (ExprIAdd (ExprCall 2 [ExprVar 0]) (ExprVar 1)) $ -- update hailstone count
            ExprUpdateVar 0 (ExprIAdd (ExprVar 0) (ExprValue (ValueInt 1))) $ -- update loop variable
            ExprValue (ValueInt 42) -- this is discarded
        ) $
        ExprVar 1
    ),
    MkFuncDef TypeInt [TypeInt] (
        ExprIf (ExprIEQ (ExprVar 0) (ExprValue (ValueInt 1))) (
            ExprValue (ValueInt 0)
        ) (
            ExprIf (ExprIEQ (ExprIMod (ExprVar 0) (ExprValue (ValueInt 2))) (ExprValue (ValueInt 0))) (
                ExprIAdd (ExprValue (ValueInt 1)) (ExprCall 2 [ExprIDiv (ExprVar 0) (ExprValue (ValueInt 2))])
            ) (
                ExprIAdd (ExprValue (ValueInt 1)) (ExprCall 2 [ExprIAdd (ExprIMul (ExprValue (ValueInt 3)) (ExprVar 0)) (ExprValue (ValueInt 1))])
            )
        )
    )
]

testHailstone_iter : IO ()
testHailstone_iter = assert_interp hailstone_iter (ValueInt 59542)

export
testHailstone_rec : IO ()
testHailstone_rec = assert_interp hailstone_rec (ValueInt 59542)

export
testMain : IO ()
testMain = do
    putStrLn "Running Toy tests..."
    test1
    testVars
    testHailstone_iter
    testHailstone_rec