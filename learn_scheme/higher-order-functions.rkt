;Exercise 1
; what the author wants is double the number ...
; 1. make twice
(define (make-twice ls)
  (letrec
      ((cons-twice (lambda (l r) (cons (car l) (cons (car l) r))))
       (make-twice-rec (lambda (lls rls)
                         (cond ((null? lls) rls)
                               (else (make-twice-rec (cdr lls) (cons-twice lls rls)))))))
    (reverse (make-twice-rec ls '()))))

; test 1.
(equal? (make-twice '()) '())
(equal? (make-twice '(1)) '(1 1))
(equal? (make-twice '(1 2 3 4)) '(1 1 2 2 3 3 4 4))

; 2. subtract-two
; the length of the result is the same as that of the shorter one
(define (subtract-two l r) (map - l r))

; test 2.
(equal? (subtract-two '(1) '()) '())
(equal? (subtract-two '(1) '(1)) '(0))