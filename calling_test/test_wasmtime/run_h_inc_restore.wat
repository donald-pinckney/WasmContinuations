;; simple.wasm
(module
    (func $print_i32 (import "imports" "print_i32") (param i32))
    (func (export "run")
        (control $run_h (i64.const 1234))
        drop
    )

    (global $run_k (mut i64) (i64.const 0))

    (func $run_h (param $k i64) (param $arg i64)
        (global.set $run_k (local.get $k))

        i32.const 42
        call $print_i32
    )

    (func $increment (export "increment") (param i32) (result i32)
        local.get 0
        i32.const 1
        i32.add
        drop

        (restore (global.get $run_k) (i64.const 1234))
        i32.const 1234
    )
)