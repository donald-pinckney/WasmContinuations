#lang racket
(require racket/control)
(require ffi/unsafe ffi/unsafe/define)


(define-ffi-definer define-test (ffi-lib "test"))

(define _PROC (_fun -> _void))
(define-test set_bad (_fun _PROC -> _void))
(define-test bar (_fun -> _void))


(define thunks (list))

(define (pushk k) (set! thunks (cons k thunks)))
(define popk (lambda ()
	(let ([head (car thunks)] 
		  [tail (cdr thunks)]) 
		(set! thunks tail)
		head
	)
))
(define stack-size (lambda () (length thunks)))

(define (fork-handler k)
	(pushk k)
	(k 'done)
)
(define fork (lambda ()
	(control k (fork-handler k))
))

(define (driver f)
	(f)
	(when (positive? (stack-size))
		(printf "Restoring thunk...~n")
		((popk) 'done)
		(printf "Done with thunk~n")
	)
)

(define (bad-forker)
	(printf "Start of forker~n")
	(fork)
	(printf "End of forker~n")
)
(define (bad)
	(printf "Start of bad~n")
	(prompt (bad-forker))
	(printf "End of bad~n")
)

(define the-main (lambda ()
	(printf "before foreign call~n")
	(bar)
	(printf "after foreign call~n")
))

(set_bad bad)

(driver the-main)

(printf "All done!~n")

; (set_bad wasm-$bad)
; (wasm-$main)