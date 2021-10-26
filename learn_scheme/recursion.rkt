; Execrise 1
; 1. my-length
(define (my-length a-list)
  (cond ((null? a-list) 0)
        (else (+ 1 (my-length (cdr a-list))))))
; test 1
(= (my-length '()) 0)
(= (my-length '(1 2 3)) 3)

; 2. summarize
(define (summerize a-list)
  (cond ((null? a-list) 0)
        (else (+ (car a-list) (summerize (cdr a-list))))))
; test 2
(= (summerize '()) 0)
(= (summerize '(1 2 3)) 6)

; 3. my-remove
(define (remove ls x)
  (cond
    ((null? ls) '())
    ((eqv? (car ls) x) (remove (cdr ls) x))
    (else (cons (car ls) (remove (cdr ls) x)))
    ))

; test 3.
(equal? (remove '(1 3 7 8 3 2 3 3) 3) '(1 7 8 2))
(equal? (remove '() '()) '())

; 4. find
; assume that x is not '()


(define (find-impl ls x count)
  (cond
    ((null? ls) #f)
    ((eqv? (car ls) x) count)
    (else (find-impl (cdr ls) x (+ count 1)))
    ))

(define (find ls x) (find-impl ls x 0))
; test 4

(eqv? (find '(1 3 7 8 3 2 3 3) 8) 3)
(eqv? (find '(1 3 7 8 3 2 3 3) 0) #f)
(eqv? (find '() 0) #f)