#lang racket
(require racket/control)
(require ffi/unsafe ffi/unsafe/define)

(define-ffi-definer define-test (ffi-lib "test"))

(define _PROC (_fun -> _void))
(define-test set_bad (_fun _PROC -> _void))
(define-test bar (_fun -> _void))



(define k1 'undefined)
(define k2 'undefined)

(define (wasm-$main)
  (control k (wasm-$h1 k)))

(define (wasm-$h1 k)
  (set! k1 k)
  (bar))

; (define (rust-bar)
;   (printf "Call to wasm-$bad from rust-bar.~n")
;   ; The line below prevents the bad behavior.
;   ; (call-with-continuation-barrier wasm-$bad)
;   (call-with-continuation-barrier (lambda () (prompt (wasm-$bad))))
;   ; (wasm-$bad)
;   ; (prompt (wasm-$bad))
;   (printf "Return from wasm-$bad to rust-bar.~n"))

(define (wasm-$bad)
  (control k (wasm-$h2 k)))

(define (wasm-$h2 k)
  (set! k2 k)
  (printf "Restoring!~n")
  (k1 'done))


(set_bad wasm-$bad)
(wasm-$main)