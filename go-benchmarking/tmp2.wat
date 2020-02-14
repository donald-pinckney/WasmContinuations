(module
  ;; (global i32 (i32.const 0))

(func $main.other_thing (type 0) (param i32) (result i32)
    (local i32)
    (local.set 1
      (global.get 0))
    (block  ;; label = @1
      (block  ;; label = @2
        (block  ;; label = @3
          (br_table 0 (;@3;) 1 (;@2;)
            (local.get 0)))
        (if  ;; label = @4
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
            (br_if 2 (;@2;))))
        (global.set 0
          (local.tee 1
            (i32.add
              (local.get 1)
              (i32.const 8))))
        (return
          (i32.const 0)))
      (unreachable))
    (i32.const 1))
  (func $main.stuff (type 0) (param i32) (result i32)
    (local i32 i64 i64 f64)
    (local.set 1
      (global.get 0))
    (block  ;; label = @1
      (loop  ;; label = @2
        (block  ;; label = @3
          (block  ;; label = @4
            (block  ;; label = @5
              (block  ;; label = @6
                (block  ;; label = @7
                  (block  ;; label = @8
                    (br_table 0 (;@8;) 0 (;@8;) 1 (;@7;) 2 (;@6;) 3 (;@5;) 4 (;@4;) 5 (;@3;)
                      (local.get 0)))
                  (if  ;; label = @9
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
                        (i64.const 381550592))
                      (call $runtime.morestack_noctxt
                        (i32.const 0))
                      (local.set 1
                        (global.get 0))
                      (br_if 7 (;@2;))))
                  (global.set 0
                    (local.tee 1
                      (i32.sub
                        (local.get 1)
                        (i32.const 24))))
                  (local.set 2
                    (i64.add
                      (i64.add
                        (i64.load offset=40
                          (local.get 1))
                        (i64.load offset=32
                          (local.get 1)))
                      (i64.const 6)))
                  (i64.store
                    (local.get 1)
                    (local.get 2))
                  (local.set 3
                    (i64.const 0))
                  (local.set 4
                    (f64.const 0x0p+0 (;=0;)))
                  (local.set 0
                    (i32.const 4))
                  (br 5 (;@4;)))
                (i64.store offset=16
                  (local.get 1)
                  (local.get 3))
                (f64.store offset=8
                  (local.get 1)
                  (local.get 4))
                (global.set 0
                  (local.tee 1
                    (i32.sub
                      (local.get 1)
                      (i32.const 8))))
                (i64.store
                  (local.get 1)
                  (i64.const 381550595))
                (call $main.other_thing
                  (i32.const 0))
                (local.set 1
                  (global.get 0))
                (br_if 5 (;@4;)))
              (local.set 3
                (i64.add
                  (i64.load offset=16
                    (local.get 1))
                  (i64.const 1)))
              (local.set 4
                (f64.add
                  (f64.load offset=8
                    (local.get 1))
                  (f64.convert_i64_s
                    (i64.add
                      (i64.mul
                        (i64.load offset=16
                          (local.get 1))
                        (i64.load offset=16
                          (local.get 1)))
                      (i64.load offset=16
                        (local.get 1))))))
              (local.set 2
                (i64.load
                  (local.get 1))))
            (if  ;; label = @10
              (i32.eqz
                (i32.wrap_i64
                  (i64.extend_i32_u
                    (i64.lt_s
                      (local.get 3)
                      (local.get 2)))))
              (then
                (local.set 0
                  (i32.const 5))
                (br 3 (;@7;))))
            (local.set 0
              (i32.const 2))
            (br 2 (;@8;)))
          (f64.store
            (i32.wrap_i64
              (i64.add
                (i64.extend_i32_u
                  (local.get 1))
                (i64.const 48)))
            (local.get 4))
          (global.set 0
            (local.tee 1
              (i32.add
                (local.get 1)
                (i32.const 24))))
          (global.set 0
            (local.tee 1
              (i32.add
                (local.get 1)
                (i32.const 8))))
          (return
            (i32.const 0))))
      (unreachable))
    (i32.const 1))
)