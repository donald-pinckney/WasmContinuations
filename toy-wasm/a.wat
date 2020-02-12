(module
	(import "console" "log_i32" (func $log_i32 (param i32)))
	(import "console" "log_f64" (func $log_f64 (param f64)))



	(func $f0  (result f64 ) 
		i64.const 100000000
		call $f1
	)
	(func $f1 (param i64) (result f64 ) (local f64) (local i64)
		f64.const 0
		set_local 1
		i64.const 0
		set_local 2
		(block
			get_local 2
			get_local 0
			i64.ge_s
			br_if 0
			(loop
				get_local 1
				get_local 2
				f64.convert_i64_s
				call $f2
				f64.add
				set_local 1
				get_local 2
				i64.const 1
				i64.add
				tee_local 2
				get_local 0
				i64.lt_s
				br_if 0
			)
		)
		get_local 1
	)
	(func $f2 (param f64) (result f64 ) (local i64)
		i64.const 0
		get_local 0
		i64.trunc_f64_s
		i64.const 1
		i64.and
		i64.sub
		i64.const 1
		i64.shl
		i64.const 1
		i64.add
		tee_local 1
		i64.const 2
		i64.shl
		f64.convert_i64_s
		get_local 0
		get_local 0
		f64.add
		f64.const 1
		f64.add
		f64.div
	)
	(func $f3  
		call $f0
		call $log_f64
	)

	(start $f3)
)