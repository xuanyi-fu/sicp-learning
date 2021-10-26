;Ex 2.28

(define (fringe ls)
  (cond ((null? ls) '())
        ((list? ls) (apply append (map fringe ls)))
        (else (list ls))))

; test

(equal? (fringe '(() (4 5 6 (7 8 9))))
        '(4 5 6 7 8 9))


(equal? (fringe '(() (() ())))
        '())



