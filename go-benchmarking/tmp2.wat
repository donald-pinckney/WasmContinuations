(func $main.stuff (type 0) (param i32) (result i32) (local i32)
    ;; Local 1 seems to be the SP, loaded from global 0
    (local.set 1
      (global.get 0))
    (block  ;; label = @1
      (block  ;; label = @2
        (block  ;; label = @3
          ;; If the function param (i32) is 0, then do nothing, else abort.
          ;; The param is 0 (see $main.main), so this does nothing?
          (br_table 0 (;@3;) 1 (;@2;)
            (local.get 0))
        )

        ;; See if the SP (local 1) is LTE the stack bottom?
        (if  ;; label = @4
          (i32.le_u
            (local.get 1)
            (i32.load offset=16
              (i32.wrap_i64
                (global.get 2))))
          (then
            ;; If so, grow the stack or split the stack???
            (global.set 0
              (local.tee 1
                (i32.sub
                  (local.get 1)
                  (i32.const 8))))
            (i64.store
              (local.get 1)
              (i64.const 381485056))
            (call $runtime.morestack_noctxt
              (i32.const 0))
            (local.set 1
              (global.get 0))
            (br_if 2 (;@2;)))
        )

        (f64.store
          ;; Address to store at
          (i32.wrap_i64
            (i64.add
              (i64.extend_i32_u
                (local.get 1))
              (i64.const 24)))
          ;; Value to store
          (f64.convert_i64_s
            (i64.add
              (i64.add
                (i64.load offset=16
                  (local.get 1))
                (i64.load offset=8
                  (local.get 1)))
              (i64.const 6))))
          
        (global.set 0
          (local.tee 1
            (i32.add
              (local.get 1)
              (i32.const 8))))

        (return (i32.const 0))
      )
      unreachable
    )
    (i32.const 1))