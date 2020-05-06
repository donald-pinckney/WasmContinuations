; (system "cc -fPIC -shared -o test.so test.c") ; If on linux
(system "cc -dynamiclib -o test.so test.c")  ; If on macOS
(load-shared-object "./test.so")

(define bar (foreign-procedure "bar" () void))
(define set_bad (foreign-procedure "set_bad" (void*) void))

(define gk1 0)

(define h1
    (lambda (k1)
        (display "Starting h1\n")
        (set! gk1 k1)
        (bar)
        (display "Ending h1\n")
    )
)

(define h2
    (lambda (k2)
        (display "Starting h2\n")
        (gk1 6)
        (display "Ending h2\n")
    )
)

(define bad 
    (lambda () 
        (display "Starting bad\n")
        (call/cc h2)
        (display "Ending bad\n")
    )
)
(define bad_ep
    (let ([x (foreign-callable bad () void)])
        (lock-object x)
        (foreign-callable-entry-point x)
    )
)
(set_bad bad_ep)

(define main
    (lambda ()
        (display "Starting main\n")
        (call/cc h1)
        (display "Ending main")
        (newline)
    )
)

(main)

; ; (display bad_ep)
; (newline)
