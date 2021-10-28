(define cl '(1 2 3 4))

(let loop((head cl) (cur cl)) 
  (cond ((null? cur) '())
        ((null? (cdr cur)) (set-cdr! cl head))
        (else (loop head (cdr cur)))))

; the following command will make chez-scheme inifite looping
; (display cl)