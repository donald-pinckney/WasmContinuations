(module
	(import "console" "log_i32" (func $log_i32 (param i32)))
	(import "console" "log_f64" (func $log_f64 (param f64)))





	(func $f0  (result i64 ) 
		i64.const 10000000
		call $f1
	)
	(func $f1 (param i64) (result i64 ) (local i64) (local i64)
		i64.const 0
		local.set 1
		i64.const 1
		local.set 2
		(block
			local.get 2
			local.get 0
			i64.le_s
			i32.eqz
			br_if 0
			(loop
				local.get 2
				call $f2
				local.get 1
				i64.add
				local.set 1
				local.get 2
				i64.const 1
				i64.add
				local.set 2
				local.get 2
				local.get 0
				i64.le_s
				br_if 0
			)
		)
		local.get 1
	)
	(func $f2 (param i64) (result i64 ) (local i64)
		i64.const 0
		local.set 1
		(block
			local.get 0
			i64.const 1
			i64.eq
			i32.eqz
			i32.eqz
			br_if 0
			(loop
				local.get 1
				i64.const 1
				i64.add
				local.set 1
				local.get 0
				i64.const 2
				i64.rem_s
				i64.const 0
				i64.eq
				(if
					(then
						local.get 0
						i64.const 2
						i64.div_s
						local.set 0
					)
					(else
						local.get 0
						i64.const 3
						i64.mul
						i64.const 1
						i64.add
						local.set 0
					)
				)
				local.get 0
				i64.const 1
				i64.eq
				i32.eqz
				br_if 0
			)
		)
		local.get 1
	)
	(func $f3  
		call $f0
		i32.wrap_i64
		call $log_i32
	)

	(start $f3)
)