;; simple.wasm
(module
    (func $print_i32 (import "imports" "print_i32") (param i32))
    (func (export "run")
        i32.const 42
        call $print_i32
    )

    (func (export "increment") (param i32) (result i32)
        i32.const 1
        local.get 0
        i32.add
    )
)