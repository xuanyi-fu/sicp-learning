(load "utils.rkt")

(define (element-of-set? elem set)
  (foldr (lambda (x rest) 
          (cond ((= elem x) #t)
                (else rest))) #f set))

(test "element-of-set?" 
  (element-of-set? 1 '(1 2 3 4))
  (not (element-of-set? 0 '(1 2 3 4)))
  (not (element-of-set? 0 '())))

(define (adjoin-set elem set)
  (cond ((element-of-set? elem set) set)
        (else (cons elem set))))

(test "adjoin-set"
  (element-of-set? 1 (adjoin-set 1 '(2)))
  (element-of-set? 1 (adjoin-set 1 '(0)))
  (element-of-set? 1 (adjoin-set 1 '(1)))
  (not (element-of-set? 0 (adjoin-set 1 '()))))

(define (set-intersection s1 s2)
  (cond ((or (null? s1) (null? s2)) '())
        (else (let ((x1 (car s1))
              (next-intersection (set-intersection (cdr s1) s2)))
          (cond ((element-of-set? x1 s2) (cons x1 next-intersection))
                (else next-intersection))))))

(test "set-intersection"
  (equal? (set-intersection '() '()) '())
  (equal? (set-intersection '(1) '()) '())
  (equal? (set-intersection '(1) '(1)) '(1))
  (equal? (set-intersection '(1 2) '(4 5 2)) '(2))
  (equal? (set-intersection '(1 2 3) '(4 5 6)) '())
  )

