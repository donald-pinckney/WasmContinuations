;; simple.wasm
(module
    (func $print_i32 (import "imports" "print_i32") (param i32))
    (func (export "run")
        i32.const 42
        call $print_i32
    )

    (func $increment (export "increment") (param i32) (result i32)
        ;; i32.const 1
        local.get 0
        ;; i32.add
        i64.extend_i32_s
        control $increment_handler
        i32.wrap_i64
    )

    (func $increment_handler (param $k i64) (param $arg i64)
        (restore (local.get $k) (i64.add (local.get $arg) (i64.const 1)))
    )
)