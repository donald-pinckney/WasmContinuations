module Main

import Test.Toy
import ToyComp
import WasmDump
import ToyAST
import WasmAST
import System
import WasmInterp

main : IO ()
main = do
    (prog :: args) <- getArgs
        | [] => putStrLn "can't happen!"
    let optim = elem "-O" args
    let wasm_mod = compile_module optim (hailstone_iter 10000000) -- 100000000

    -- case interp_module wasm_mod of
    --     (Left l) => putStrLn l
    --     (Right r) => putStrLn (show r)

    let wasm_txt = dump_module wasm_mod
    Right () <- writeFile "toy.wat" wasm_txt
        | Left err => (putStrLn $ "Error writing file: " ++ show err)
    n <- system "wat2wasm toy.wat -o toy.wasm"
    if n == 0 then pure () else putStrLn $ "Unexpected wat2wasm exit code: " ++ show n
