; Exercise 2
; 1. reverse
(define (my-reverse-rec ls rls)
  (cond ((null? ls) rls)
        (else (my-reverse-rec
               (cdr ls)
               (cons (car ls) rls)))))
(define (my-reverse a-list) (my-reverse-rec a-list '()))

; test 1.
(equal? (my-reverse '()) '())
(equal? (my-reverse '(1)) '(1))
(equal? (my-reverse '(1 2 3)) '(3 2 1))

; 2.summerize
(define (sum-rec ls sum)
  (cond ((null? ls) sum)
        (else (sum-rec (cdr ls) (+ (car ls) sum)))))
(define (my-sum ls) (sum-rec ls 0))

; test 2.
(eqv? (my-sum '()) 0)
(eqv? (my-sum '(1)) 1)
(eqv? (my-sum '(1 2 3)) 6)

; 3. number convert
(define (list-to-int-rec ls n)
  (cond ((null? ls) n)
        (else (list-to-int-rec (cdr ls) (+ (* 10 n) (car ls))))))
(define (string-to-int str)
  (let* ((char-ls (string->list str))
         (int-ls (map char->integer char-ls))
         (number-ls (map (lambda (n) (- n 48)) int-ls)))
    (list-to-int-rec number-ls 0)))
; test 3.
(eqv? (string-to-int "") 0)
(eqv? (string-to-int "1") 1)
(eqv? (string-to-int "1234") 1234)