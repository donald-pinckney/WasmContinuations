(module
	(import "console" "log_i32" (func $log_i32 (param i32)))
	(import "console" "log_f64" (func $log_f64 (param f64)))

	(global (mut i32) (i32.const 0))

	(memory 1)

	(func $f0  (local i32)
		global.get 0
		local.tee 0
		i32.const -8
		i32.add
		local.get 0
		i32.const 24
		i32.sub
		i64.const 100000000
		i64.store offset=0
		local.get 0
		i32.const 24
		i32.sub
		global.set 0
		call $f1
		global.get 0
		local.tee 0
		i32.const 8
		i32.sub
		f64.load offset=0
		local.get 0
		i32.const 0
		i32.add
		local.tee 0
		global.set 0
		f64.store offset=0
	)
	(func $f1  (local i32)
		global.get 0
		local.tee 0
		i32.const 16
		i32.add
		local.get 0
		f64.const 0
		f64.store offset=8
		local.get 0
		i64.const 0
		i64.store offset=16
		(block
			local.get 0
			i64.load offset=16
			local.get 0
			i64.load offset=0
			i64.lt_s
			i32.eqz
			br_if 0
			(loop
				local.get 0
				local.get 0
				f64.load offset=8
				local.get 0
				i32.const 16
				i32.sub
				local.get 0
				i64.load offset=16
				f64.convert_i64_s
				f64.store offset=0
				local.get 0
				i32.const 16
				i32.sub
				global.set 0
				call $f2
				global.get 0
				local.tee 0
				i32.const 8
				i32.sub
				f64.load offset=0
				f64.add
				f64.store offset=8
				local.get 0
				local.get 0
				i64.load offset=16
				i64.const 1
				i64.add
				i64.store offset=16
				local.get 0
				i64.load offset=16
				local.get 0
				i64.load offset=0
				i64.lt_s
				br_if 0
			)
		)
		local.get 0
		f64.load offset=8
		local.get 0
		i32.const 24
		i32.add
		local.tee 0
		global.set 0
		f64.store offset=0
	)
	(func $f2  (local i32)
		global.get 0
		local.tee 0
		i32.const 8
		i32.add
		local.get 0
		i64.const 0
		local.get 0
		f64.load offset=0
		i64.trunc_f64_s
		i64.const 2
		i64.rem_s
		i64.sub
		i64.const 2
		i64.mul
		i64.const 1
		i64.add
		i64.store offset=8
		local.get 0
		i64.load offset=8
		i64.const 4
		i64.mul
		f64.convert_i64_s
		local.get 0
		f64.load offset=0
		f64.const 2
		f64.mul
		f64.const 1
		f64.add
		f64.div
		local.get 0
		i32.const 16
		i32.add
		local.tee 0
		global.set 0
		f64.store offset=0
	)
	(func $f3  
		i32.const 65536
		global.set 0
		call $f0
		global.get 0
		i32.const 8
		i32.sub
		f64.load offset=0
		call $log_f64
	)

	(start $f3)
)