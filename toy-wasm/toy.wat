(module
	(import "console" "log_i32" (func $log_i32 (param i32)))
	(import "console" "log_f64" (func $log_f64 (param f64)))
	(func $f0  (result i64 ) (local i64) (local i64)
		i64.const 3
		i64.const 6
		i64.add
		set_local 0
		get_local 0
		i64.const 5
		i64.sub
		set_local 1
		get_local 1
		i64.const 1
		i64.add
		set_local 1
		get_local 0
		get_local 1
		i64.rem_s
	)
	(func $start
		call $f0
		i32.wrap_i64
		call $log_i32
	)
	(start $start)
)