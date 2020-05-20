#lang racket
(require racket/control)



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
	; (bad-forker)
	(printf "End of bad~n")
)

;; This is the "foreign function"
(define (bar)
	(printf "Starting bar <--------- ~n")
	(bad)
	(printf "Ending bar <--------- ~n")
)

(define the-main (lambda ()
	(printf "before foreign call~n")
	(bar)
	(printf "after foreign call~n")
))

(define (main k)
	(driver the-main)
)


(control k (main k))

(printf "All done!~n")
