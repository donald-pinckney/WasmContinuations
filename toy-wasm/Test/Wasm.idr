module Test.Wasm

import WasmAST
import WasmInterp

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

export
testMain : IO ()
testMain = do
    putStrLn "Running Wasm tests..."
