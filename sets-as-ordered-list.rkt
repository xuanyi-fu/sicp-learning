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

(define (set-intersection s1 s2)
  (cond ((or (null? s1) (null? s2)) '())
        (else (let ((x1 (car s1)) (x2 (car s2)))
                (cond ((< x1 x2) (set-intersection (cdr s1) s2))
                      ((< x2 x1) (set-intersection s1 (cdr s2)))
                      (else (cons x1 (set-intersection (cdr s1) (cdr s2)))))))))

(test "set-intersection"
  (equal? (set-intersection '() '()) '())
  (equal? (set-intersection '(1) '()) '())
  (equal? (set-intersection '(1) '(1)) '(1))
  (equal? (set-intersection '(1 2) '(2 4 5)) '(2))
  (equal? (set-intersection '(1 2 3) '(4 5 6)) '())

  (ordered-unique-list? (set-intersection '() '()))
  (ordered-unique-list? (set-intersection '(1) '()))
  (ordered-unique-list? (set-intersection '(1) '(1)))
  (ordered-unique-list? (set-intersection '(1 2) '(2 4 5)))
  (ordered-unique-list? (set-intersection '(1 4 5 6) '(4 5 6)))
)

(define (set-union-n-square s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        (else 
          (let* ((x1 (car s1)) 
                 (rest (set-union (cdr s1) s2))
                 (x2 (car rest)))
            (cond ((< x1 x2) (cons x1 rest))
                  ((= x1 x2) rest)
                  (else (adjoin-set x1 rest)))))))

(define (set-union s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        (else 
          (let ((x1 (car s1)) (x2 (car s2)))
            (cond ((= x1 x2) (cons x1 (set-union (cdr s1) (cdr s2))))
                  ((< x1 x2) (cons x1 (set-union (cdr s1) s2)))
                  (else (cons x2 (set-union s1 (cdr s2)))))))))

(test "set-union"
  (equal? (set-union '() '()) '())
  (equal? (set-union '(1) '()) '(1))
  (equal? (set-union '(1) '(1)) '(1))
  (equal? (set-union '(1 2) '(2 4 5)) '(1 2 4 5))
  (equal? (set-union '(3 4) '(1 2 3 4 5)) '(1 2 3 4 5))
  (equal? (set-union '(1) '(-1)) '(-1 1))

  (ordered-unique-list? (set-union '() '()))
  (ordered-unique-list? (set-union '(1) '()))
  (ordered-unique-list? (set-union '(1) '(1)))
  (ordered-unique-list? (set-union '(1 2) '(2 4 5)))
  (ordered-unique-list? (set-union '(3 4) '(1 2 3 4 5)))
)

