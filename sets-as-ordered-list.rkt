(load "utils.rkt")

(define (element-of-set? elem set) 
  (foldr (lambda (x rest) 
          (cond ((= elem x) #t)
                ((< elem x) #f)
                (else rest))) #f set))

(test "element-of-set?" 
  (element-of-set? 1 '(1 2 3 4))
  (not (element-of-set? 0 '(1 2 3 4)))
  (not (element-of-set? 0 '())))

(define (ordered-unique-list? ls)
  (cond ((or (null? ls) (null? (cdr ls))) #t)
        (else 
          (and (< (car ls) (cadr ls)) 
               (ordered-unique-list? (cdr ls))))))

(test "ordered-unique-list?" 
  (ordered-unique-list? '())
  (ordered-unique-list? '(1))
  (ordered-unique-list? '(1 2))
  (not (ordered-unique-list? '(1 3 2)))
  (not (ordered-unique-list? '(1 1))))

(define (adjoin-set elem set) 
  (cond ((null? set) (cons elem set))
        ((< elem (car set)) (cons elem set))
        ((= elem (car set)) (cons elem (cdr set)))
        (else (cons (car set) (adjoin-set elem (cdr set))))))

(test "adjoin-set"
  (element-of-set? 1 (adjoin-set 1 '(2)))
  (element-of-set? 1 (adjoin-set 1 '(0)))
  (element-of-set? 1 (adjoin-set 1 '(1)))
  (not (element-of-set? 0 (adjoin-set 1 '())))
  (ordered-unique-list? (adjoin-set 2 '(1 3)))
  (ordered-unique-list? (adjoin-set 2 '(2)))
  (ordered-unique-list? (adjoin-set 2 '(3)))
  (ordered-unique-list? (adjoin-set 1 '(0)))
  (equal? (adjoin-set 2 '(1 3)) '(1 2 3))
  (equal? (adjoin-set 2 '(2)) '(2))
  (equal? (adjoin-set 2 '(3)) '(2 3))
  (equal? (adjoin-set 1 '(0)) '(0 1)))

