module Misc.Main

import LangDefs.ToyAST
import LangDefs.WasmAST

import Frontend.Tokenizer
import Frontend.Parser
import Frontend.TypeCheck

import Backend.Compiler
import Backend.Optimizer
import Backend.WasmDump

import System
import Misc.ArgParse

%default covering

compilerPipeline : (optim : Bool) -> (heap_stack : Bool) -> String -> Either String WasmModule
compilerPipeline optim heap_stack str = do
    toks <- tokenize str
    p <- parseModule toks
    ast <- typeCheckModule p
    let mod = compile_module heap_stack ast
    let optim_mod = (if optim then optimize_module else id) mod
    pure optim_mod


record ArgumentConfig where
    constructor MkArgumentConfig
    in_f : String
    out_name : String
    do_optim : Bool
    heap_stack : Bool

out_wat_f : ArgumentConfig -> String
out_wat_f x = (out_name x) ++ ".wat"

out_wasm_f : ArgumentConfig -> String
out_wasm_f x = (out_name x) ++ ".wasm"

main : IO ()
main = do
    Right config <- parseArgs (do
        do_optim <- matchFlag "-O"
        heap_stack <- matchFlag "--heap-stack"
        out_f <- matchDefaultOption "-o" "a"
        in_f <- assertOnlyArgument
        pure $ MkArgumentConfig in_f out_f do_optim heap_stack
    ) | Left err => putStrLn ("Error parsing arguments: " ++ err)

    Right prog_str <- readFile (in_f config)
        | Left err => putStrLn ("Error reading input file '" ++ (in_f config) ++ "': " ++ show err)

    let Right mod = compilerPipeline (do_optim config) (heap_stack config) prog_str
        | Left err => putStrLn ("Error: " ++ err)

    let wasm_txt = dump_module mod
    Right () <- writeFile (out_wat_f config) wasm_txt
        | Left err => putStrLn ("Error writing file '" ++ (out_wat_f config) ++ "': " ++ show err)

    n <- system $ "wat2wasm " ++ (out_wat_f config) ++ " -o " ++ (out_wasm_f config)
    if n == 0 then pure () else putStrLn $ "Error: unexpected wat2wasm exit code: " ++ show n
