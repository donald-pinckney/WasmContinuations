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
		i64.const 10000000
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
		i64.load offset=0
		local.get 0
		i32.const 0
		i32.add
		local.tee 0
		global.set 0
		i64.store offset=0
	)
	(func $f1  (local i32)
		global.get 0
		local.tee 0
		i32.const 16
		i32.add
		local.get 0
		i64.const 0
		i64.store offset=8
		local.get 0
		i64.const 1
		i64.store offset=16
		(block
			local.get 0
			i64.load offset=16
			local.get 0
			i64.load offset=0
			i64.gt_s
			br_if 0
			(loop
				local.get 0
				local.get 0
				i32.const 8
				i32.sub
				local.get 0
				i64.load offset=16
				i64.store offset=0
				local.get 0
				i32.const 8
				i32.sub
				global.set 0
				call $f2
				global.get 0
				local.tee 0
				i32.const 8
				i32.sub
				i64.load offset=0
				local.get 0
				i64.load offset=8
				i64.add
				i64.store offset=8
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
				i64.le_s
				br_if 0
			)
		)
		local.get 0
		i64.load offset=8
		local.get 0
		i32.const 24
		i32.add
		local.tee 0
		global.set 0
		i64.store offset=0
	)
	(func $f2  (local i32)
		global.get 0
		local.tee 0
		i32.const 0
		i32.add
		local.get 0
		i64.load offset=0
		i64.const 1
		i64.eq
		(if (result i64)
			(then
				i64.const 0
			)
			(else
				local.get 0
				i64.load offset=0
				i64.const 1
				i64.and
				i64.eqz
				(if (result i64)
					(then
						i64.const 1
						local.get 0
						i32.const 8
						i32.sub
						local.get 0
						i64.load offset=0
						i64.const 1
						i64.shr_u
						i64.store offset=0
						local.get 0
						i32.const 8
						i32.sub
						global.set 0
						call $f2
						global.get 0
						local.tee 0
						i32.const 8
						i32.sub
						i64.load offset=0
						i64.add
					)
					(else
						i64.const 1
						local.get 0
						i32.const 8
						i32.sub
						local.get 0
						i64.load offset=0
						i64.const 3
						i64.mul
						i64.const 1
						i64.add
						i64.store offset=0
						local.get 0
						i32.const 8
						i32.sub
						global.set 0
						call $f2
						global.get 0
						local.tee 0
						i32.const 8
						i32.sub
						i64.load offset=0
						i64.add
					)
				)
			)
		)
		local.get 0
		i32.const 8
		i32.add
		local.tee 0
		global.set 0
		i64.store offset=0
	)
	(func $f3  
		i32.const 65536
		global.set 0
		call $f0
		global.get 0
		i32.const 8
		i32.sub
		i64.load offset=0
		i32.wrap_i64
		call $log_i32
	)

	(start $f3)
)