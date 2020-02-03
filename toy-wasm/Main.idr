module Main

import LangDefs.ToyAST
import LangDefs.WasmAST

import Frontend.Tokenizer
import Frontend.Parser
import Frontend.TypeCheck

import Backend.Compiler
import Backend.Optimizer
import Backend.WasmDump

import System

compilerPipeline : Bool -> String -> Either String WasmModule
compilerPipeline optim str = do
    toks <- tokenize str
    p <- parseModule toks
    ast <- typeCheckModule p
    let mod = compile_module ast
    let optim_mod = (if optim then optimize_module else id) mod
    pure optim_mod

parseOutFile : List String -> (String, List String)
parseOutFile [] = ("a", [])
parseOutFile (x :: []) = ("a", [x])
parseOutFile ("-o" :: (f :: xs)) = (f, xs)
parseOutFile (x :: xs) =
    let (out, args') = parseOutFile xs in
    (out, x :: args')

parseOptimFlag : List String -> (Bool, List String)
parseOptimFlag [] = (False, [])
parseOptimFlag ("-O" :: xs) = (True, xs)
parseOptimFlag (x :: xs) =
    let (o, args') = parseOptimFlag xs in
    (o, x :: args')

main : IO ()
main = do
    (prog :: args) <- getArgs
        | [] => assert_unreachable

    let (do_optim, args) = parseOptimFlag args
    let (out_f, [in_f]) = parseOutFile args
        | (out_f, otherwise) => putStrLn ("Unrecognized arguments: " ++ show otherwise)
    let out_wat_f = out_f ++ ".wat"
    let out_wasm_f = out_f ++ ".wasm"

    Right prog_str <- readFile in_f
        | Left err => putStrLn ("Error reading input file '" ++ in_f ++ "': " ++ show err)

    let Right mod = compilerPipeline do_optim prog_str
        | Left err => putStrLn ("Error: " ++ err)

    let wasm_txt = dump_module mod
    Right () <- writeFile out_wat_f wasm_txt
        | Left err => putStrLn ("Error writing file '" ++ out_wat_f ++ "': " ++ show err)

    n <- system $ "wat2wasm " ++ out_wat_f ++ " -o " ++ out_wasm_f
    if n == 0 then pure () else putStrLn $ "Error: unexpected wat2wasm exit code: " ++ show n
