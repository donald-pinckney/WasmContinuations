(module
	(import "console" "log_i32" (func $log_i32 (param i32)))
	(import "console" "log_f64" (func $log_f64 (param f64)))
	(func $f0  (result i64 ) 
		i64.const 10000000
		call $f1
	)
	(func $f1 (param i64) (result i64 ) (local i64) (local i64)
		i64.const 0
		set_local 1
		i64.const 1
		set_local 2
		(block
			get_local 2
			get_local 0
			i64.le_s
			i32.eqz
			br_if 0
			(loop
				get_local 2
				call $f2
				get_local 1
				i64.add
				set_local 1
				get_local 2
				i64.const 1
				i64.add
				set_local 2
				i64.const 42
				get_local 2
				get_local 0
				i64.le_s
				br_if 0
				br 1
			)
		)
		get_local 1
	)
	(func $f2 (param i64) (result i64 ) (local i64)
		i64.const 0
		set_local 1
		(block
			get_local 0
			i64.const 1
			i64.eq
			i32.eqz
			i32.eqz
			br_if 0
			(loop
				get_local 1
				i64.const 1
				i64.add
				set_local 1
				get_local 0
				i64.const 1
				i64.and
				i64.const 0
				i64.eq
				(if (result i64)
					(then
						get_local 0
						i64.const 1
						i64.shr_u
						set_local 0
						i64.const 42
					)
					(else
						i64.const 3
						get_local 0
						i64.mul
						i64.const 1
						i64.add
						set_local 0
						i64.const 42
					)
				)
				get_local 0
				i64.const 1
				i64.eq
				i32.eqz
				br_if 0
				br 1
			)
		)
		get_local 1
	)
	(func $start
		call $f0
		i32.wrap_i64
		call $log_i32
	)
	(start $start)
)