#lang racket
(require racket/control)

(define k1 'undefined)
(define k2 'undefined)

(define (rust-foo)
  (wasm-$main))

(define (wasm-$main)
  (control k (wasm-$h1 k)))

(define (wasm-$h1 k)
  (set! k1 k)
  (rust-bar))

(define (rust-bar)
  (printf "Call to wasm-$bad from rust-bar.~n")
  ; The line below prevents the bad behavior.
  (call-with-continuation-barrier wasm-$bad)
  (printf "Return from wasm-$bad to rust-bar.~n"))

(define (wasm-$bad)
  (control k (wasm-$h2 k)))

(define (wasm-$h2 k)
  (set! k2 k)
  (k1 'done))

(rust-foo)