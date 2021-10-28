(define cl '(1 2 3 4))

(let loop((head cl) (cur cl)) 
  (cond ((null? cur) '())
        ((null? (cdr cur)) (set-cdr! cl head))
        (else (loop head (cdr cur)))))

; the following command will make chez-scheme inifite looping
; (display cl)


; find the element from which the cycle begins
(let loop((fast cl) (slow cl))
  (cond ((= (car fast) (car slow)) (display (car fast)))
        (else (loop (cddr fast) (cdr slow)))))