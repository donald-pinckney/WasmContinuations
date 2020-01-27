(module
	(import "console" "log_i32" (func $log_i32 (param i32)))
	(import "console" "log_f64" (func $log_f64 (param f64)))
	(func $f0  (result i64 )
		i64.const 10000000
		call $hailstones
	)
	(func $hailstones (param i64) (result i64) (local i64) (local i64)

		(set_local 1 (i64.const 0))
		(set_local 2 (i64.const 1))

		(block
			(loop

				(call $hailstone (get_local 2))
				get_local 1
				i64.add
				set_local 1

				(set_local 2 (i64.add (get_local 2) (i64.const 1)))

				(i64.le_s (get_local 2) (get_local 0))
				br_if 0
			)
		)

		get_local 1
	)
	(func $hailstone (param i64) (result i64) (local i64)
		(set_local 1 (i64.const 0))

		(block
			(i64.eq (get_local 0) (i64.const 1))
			br_if 0

			(loop

				;; (i64.eq (i64.rem_s (get_local 0) (i64.const 2)) (i64.const 0))
				(i64.eq (i64.and (get_local 0) (i64.const 1)) (i64.const 0))
				(if
					(then
						(set_local 0 (i64.shr_u (get_local 0) (i64.const 1)))
						;; (set_local 0 (i64.div_s (get_local 0) (i64.const 2)))

					)
					(else
						(set_local 0 (i64.add (i64.mul (i64.const 3) (get_local 0)) (i64.const 1)))
					)
				)

				(set_local 1 (i64.add (get_local 1) (i64.const 1)))

				(i64.ne (get_local 0) (i64.const 1))
				br_if 0
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
