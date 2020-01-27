module Test.Wasm

import WasmAST
import WasmInterp
import ToyAST
import ToyComp
import Test.Toy

assert_interp : WasmModule -> WasmValue -> IO ()
assert_interp m expected = case interp_module m of
    (Left err) => putStrLn $ "Test failed, error during interpretation: " ++ err
    (Right v) => if v == expected
        then putStrLn "Test passed"
        else putStrLn $ "Test failed, expected: " ++ show expected ++ ", received: " ++ show v

assert_interp_fail : WasmModule -> IO ()
assert_interp_fail m = case interp_module m of
    (Left err) => putStrLn "Test passed"
    (Right v) => putStrLn $ "Test failed, expected interpretation failure, received result: " ++ show v

assert_compile : Module nmfns -> WasmValue -> IO ()
assert_compile m expected =
    let m' = compile_module m in
    assert_interp m' expected

export
testMain : IO ()
testMain = do
    putStrLn "Running Wasm tests..."
    assert_compile toy1 (WasmValueI64 3)
    assert_compile varsProg (WasmValueI64 4)
    -- assert_compile hailstone_iter (WasmValueI64 59542)
    -- assert_compile hailstone_rec (WasmValueI64 59542)
