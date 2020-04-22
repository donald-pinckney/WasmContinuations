;; simple.wasm
(module
    (func $print_i32 (import "imports" "print_i32") (param i32))
    (func (export "run")
        (control $run_h (i64.const 1234))
        drop
    )

    (func $run_h (param $k i64) (param $arg i64)
        i32.const 42
        call $print_i32
        (restore (local.get $k) (i64.const 1234))
    )

    (func $increment (export "increment") (param i32) (result i32)
        local.get 0
        i32.const 1
        i32.add
    )
)