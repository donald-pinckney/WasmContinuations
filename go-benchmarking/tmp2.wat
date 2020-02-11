;;                                ???                       sp z   i       s
(func $main.stuff (type 0) (param i32) (result i32)
    (local i32 i64 i64 i64 f64)
    (local.set 1
      (global.get 0))
    (block  ;; label = @1
      (loop  ;; label = @2
        (block  ;; label = @3
          (block  ;; label = @4
            (block  ;; label = @5
              (block  ;; label = @6
                (block  ;; label = @7
                  (br_table 0 (;@7;) 1 (;@6;) 2 (;@5;) 3 (;@4;) 4 (;@3;)
                    (local.get 0)))
                (if  ;; label = @8
                  (i32.le_u
                    (local.get 1)
                    (i32.load offset=16
                      (i32.wrap_i64
                        (global.get 2))))
                  (then
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
                    (br_if 6 (;@2;))))
                (local.set 2
                  (i64.add
                    (i64.add
                      (i64.load offset=16
                        (local.get 1))
                      (i64.load offset=8
                        (local.get 1)))
                    (i64.const 6)))
                (local.set 3
                  (i64.const 0))
                (local.set 5
                  (f64.const 0x0p+0 (;=0;)))
                (local.set 0
                  (i32.const 2))
                (br 4 (;@4;)))
              (local.set 4
                (i64.add
                  (local.get 3)
                  (i64.const 1)))
              (local.set 5
                (f64.add
                  (local.get 5)
                  (f64.convert_i64_s
                    (i64.add
                      (i64.mul
                        (local.get 3)
                        (local.get 3))
                      (local.get 3)))))
              (local.set 3
                (local.get 4)))
            (if  ;; label = @9
              (i32.eqz
                (i32.wrap_i64
                  (i64.extend_i32_u
                    (i64.lt_s
                      (local.get 3)
                      (local.get 2)))))
              (then
                (local.set 0
                  (i32.const 3))
                (br 3 (;@6;))))
            (local.set 0
              (i32.const 1))
            (br 2 (;@7;)))
          (f64.store
            (i32.wrap_i64
              (i64.add
                (i64.extend_i32_u
                  (local.get 1))
                (i64.const 24)))
            (local.get 5))
          (global.set 0
            (local.tee 1
              (i32.add
                (local.get 1)
                (i32.const 8))))
          (return
            (i32.const 0))))
      (unreachable))
    (i32.const 1))