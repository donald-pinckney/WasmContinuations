(module
	(import "console" "log_i32" (func $log_i32 (param i32)))
	(import "console" "log_f64" (func $log_f64 (param f64)))
	(memory 1)

	(func $f0  (result i64 )
		i64.const 10000000
		call $hailstones
	)
	(func $hailstones (param i64) (result i64) (local i64) (local i64)
		(i64.store (i32.const 0) (get_local 0))

		(i64.store (i32.const 8) (i64.const 0))
		(i64.store (i32.const 16) (i64.const 1))

		;;(set_local 1 (i64.const 0))
		;;(set_local 2 (i64.const 1))

		;; local 0 -> addr 0
		;; local 1 -> addr 8
		;; local 2 -> addr 16

		(block
			(loop

				(i32.const 8)
				(call $hailstone (i64.load (i32.const 16)))
				;;get_local 1
				(i64.load (i32.const 8))
				i64.add
				;;set_local 1
				i64.store


				(i64.store (i32.const 16) (i64.add (i64.load (i32.const 16)) (i64.const 1)))

				(i64.le_s (i64.load (i32.const 16)) (i64.load (i32.const 0)))
				br_if 0
			)
		)

		(i64.load (i32.const 8))
	)
	(func $hailstone (param i64) (result i64) (local i64)
		(i64.store (i32.const 24) (get_local 0))

		;;(set_local 1 (i64.const 0))
		(i64.store (i32.const 32) (i64.const 0))

		;; local 0 -> addr 24
		;; local 1 -> addr 32

		(block
			(i64.eq (i64.load (i32.const 24)) (i64.const 1))
			br_if 0

			(loop

				;; (i64.eq (i64.rem_s (get_local 0) (i64.const 2)) (i64.const 0))
				(i64.eq (i64.and (i64.load (i32.const 24)) (i64.const 1)) (i64.const 0))
				(if
					(then
						;;(set_local 0 (i64.shr_u (get_local 0) (i64.const 1)))
						(i64.store (i32.const 24) (i64.shr_u (i64.load (i32.const 24)) (i64.const 1)))
						;; (set_local 0 (i64.div_s (get_local 0) (i64.const 2)))

					)
					(else
						(i64.store (i32.const 24) (i64.add (i64.mul (i64.const 3) (i64.load (i32.const 24))) (i64.const 1)))
						;;(set_local 0 (i64.add (i64.mul (i64.const 3) (get_local 0)) (i64.const 1)))
					)
				)

				;;(set_local 1 (i64.add (i64.load (i32.const 32)) (i64.const 1)))
				(i64.store (i32.const 32) (i64.add (i64.load (i32.const 32)) (i64.const 1)))

				(i64.ne (i64.load (i32.const 24)) (i64.const 1))
				br_if 0
			)
		)

		(i64.load (i32.const 32))
	)
	(func $start
		call $f0
		i32.wrap_i64
		call $log_i32
	)
	(start $start)
)
