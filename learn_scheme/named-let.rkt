; Exercise 3
; 1.1 Q3 of Exercise 1
(define (remove-tail-rec ls x)
  (reverse (let loop((lls ls) (rls '()))
    (cond
      ((null? lls) rls)
      ((eqv? (car lls) x) (loop (cdr lls) rls))
      (else (loop (cdr lls) (cons (car lls) rls)))))))
; not tail-rec
; i think this is nonsense
(define (remove ls x)
  (let loop((lls ls))
    (cond ((null? lls) '())
          ((eqv? (car lls) x) (loop (cdr lls)))
          (else (cons (car lls) (loop (cdr lls)))))))
; test 1.1
(equal? (remove-tail-rec '(1 3 7 8 3 2 3 3) 3) '(1 7 8 2))
(equal? (remove-tail-rec '() '()) '())
(equal? (remove '(1 3 7 8 3 2 3 3) 3) '(1 7 8 2))
(equal? (remove '() '()) '())

; 1.2 Q4 of Exercise 1
(define (find ls x)
  (let loop((lls ls) (count 0))
    (cond ((null? lls) #f)
          ((eqv? (car lls) x) count)
          (else (loop (cdr lls) (+ count 1))))))
; test 1.2
(eqv? (find '(1 3 7 8 3 2 3 3) 8) 3)
(eqv? (find '(1 3 7 8 3 2 3 3) 0) #f)
(eqv? (find '() 0) #f)