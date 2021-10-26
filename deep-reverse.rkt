(define (deep-reverse ls)
  (cond ((null? ls) '())
        ((list? ls) (reverse (map deep-reverse ls)))
        (else ls)))
;test

(equal? (deep-reverse '((1 2 3) 1 2 3))
        '(3 2 1 (3 2 1)))

(equal? (deep-reverse '((1 2 (3 4)) 1 2 3))
        '(3 2 1 ((4 3) 2 1)))

(equal? (deep-reverse '(() 1 2 3)) '(3 2 1 ()))

(equal? (deep-reverse '()) '())
