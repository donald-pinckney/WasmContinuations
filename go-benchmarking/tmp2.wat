(func $main.other_thing (type 0) (param i32) (result i32)
    (local i32)
    (local.set 1
      (global.get 0))
    (block  ;; label = @1
      (block  ;; label = @2
        (block  ;; label = @3
          (block  ;; label = @4
            (br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
              (local.get 0)))
          (if  ;; label = @5
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
              (br_if 3 (;@2;))))
          (global.set 0
            (local.tee 1
              (i32.sub
                (local.get 1)
                (i32.const 8))))
          (i64.store
            (local.get 1)
            (i64.const 381485057))
          (call $runtime.Gosched
            (i32.const 0))
          (local.set 1
            (global.get 0))
          (br_if 2 (;@3;)))
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
                    (block  ;; label = @9
                      (br_table 0 (;@9;) 0 (;@9;) 1 (;@8;) 2 (;@7;) 3 (;@6;) 4 (;@5;) 5 (;@4;) 6 (;@3;)
                        (local.get 0)))
                    (if  ;; label = @10
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
                        (br_if 8 (;@2;))))
                    (global.set 0
                      (local.tee 1
                        (i32.sub
                          (local.get 1)
                          (i32.const 48))))
                    (local.set 2
                      (i64.add
                        (i64.add
                          (i64.load offset=72
                            (local.get 1))
                          (i64.load offset=64
                            (local.get 1)))
                        (i64.const 6)))
                    (i64.store offset=16
                      (local.get 1)
                      (local.get 2))
                    (local.set 3
                      (i64.const 0))
                    (local.set 4
                      (f64.const 0x0p+0 (;=0;)))
                    (local.set 0
                      (i32.const 4))
                    (br 6 (;@4;)))
                  (i64.store offset=32
                    (local.get 1)
                    (local.get 3))
                  (f64.store offset=24
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
                  (br_if 6 (;@4;)))
                (local.set 3
                  (i64.add
                    (i64.load offset=32
                      (local.get 1))
                    (i64.const 1)))
                (local.set 4
                  (f64.add
                    (f64.load offset=24
                      (local.get 1))
                    (f64.convert_i64_s
                      (i64.add
                        (i64.mul
                          (i64.load offset=32
                            (local.get 1))
                          (i64.load offset=32
                            (local.get 1)))
                        (i64.load offset=32
                          (local.get 1))))))
                (local.set 2
                  (i64.load offset=16
                    (local.get 1))))
              (if  ;; label = @11
                (i32.eqz
                  (i32.wrap_i64
                    (i64.extend_i32_u
                      (i64.lt_s
                        (local.get 3)
                        (local.get 2)))))
                (then
                  (local.set 0
                    (i32.const 5))
                  (br 4 (;@7;))))
              (local.set 0
                (i32.const 2))
              (br 3 (;@8;)))
            (f64.store
              (i32.wrap_i64
                (i64.add
                  (i64.extend_i32_u
                    (local.get 1))
                  (i64.const 40)))
              (local.get 4))
            (i64.store
              (local.get 1)
              (i64.load offset=56
                (local.get 1)))
            (i64.store offset=8
              (local.get 1)
              (i64.add
                (i64.extend_i32_u
                  (local.get 1))
                (i64.const 40)))
            (global.set 0
              (local.tee 1
                (i32.sub
                  (local.get 1)
                  (i32.const 8))))
            (i64.store
              (local.get 1)
              (i64.const 381550598))
            (call $runtime.chansend1
              (i32.const 0))
            (local.set 1
              (global.get 0))
            (br_if 3 (;@8;)))
          (global.set 0
            (local.tee 1
              (i32.add
                (local.get 1)
                (i32.const 48))))
          (global.set 0
            (local.tee 1
              (i32.add
                (local.get 1)
                (i32.const 8))))
          (return
            (i32.const 0))))
      (unreachable))
    (i32.const 1))