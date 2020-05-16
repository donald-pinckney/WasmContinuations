#lang racket
(require racket/control)

(define k1 'undefined)
(define k2 'undefined)

(define (wasm-$main)
  (control k (wasm-$h1 k))
  (printf "All done with main.~n")
)

(define (wasm-$h1 k)
  (set! k1 k)
  (bar))

(define (bar)
  (printf "Call to wasm-$bad from rust-bar.~n")
  ; The line below prevents the bad behavior.
  ; (call-with-continuation-barrier wasm-$bad)
  ; (call-with-continuation-barrier (lambda () (prompt (wasm-$bad))))
  ; (wasm-$bad)
  (prompt (wasm-$bad))
  (printf "Return from wasm-$bad to rust-bar.~n"))

(define (wasm-$bad)
  (control k (wasm-$h2 k)))

(define (wasm-$h2 k)
  (set! k2 k)
  (printf "Restoring!~n")
  (k1 'done))


(wasm-$main)
