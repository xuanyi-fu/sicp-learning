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

