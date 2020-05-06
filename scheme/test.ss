
(system "clang -dynamiclib -o test.so test.c")
(load-shared-object "./test.so")

(define bar (foreign-procedure "bar" () void))
(define set_bad (foreign-procedure "set_bad" (void*) void))

(define bad 
    (lambda () 
        (display "Inside bad")
    ))
(define bad_ep
    (let ([x (foreign-callable bad () void)])
        (lock-object x)
        (foreign-callable-entry-point x)
    )
)
(set_bad bad_ep)

(define main
    (lambda ()
        (bar)
        (newline)
    )
)

(main)

; ; (display bad_ep)
; (newline)
