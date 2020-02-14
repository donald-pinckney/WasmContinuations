(module
	(import "console" "log_i32" (func $log_i32 (param i32)))
	(import "console" "log_f64" (func $log_f64 (param f64)))





	(func $f0  (result i64 ) 
		i64.const 100000000
		i64.const 1234
		call $f1
	)
	(func $f1 (param i64) (param i64) (result i64 ) (local i64)
		local.get 0
		local.get 1
		i64.add
		local.set 2
		local.get 0
		local.get 1
		i64.add
	)
	(func $f2  
		call $f0
		i32.wrap_i64
		call $log_i32
	)

	(start $f2)
)