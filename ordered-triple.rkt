(define (foldr op initial sequence) 
  (if (null? sequence) 
      initial 
      (op (car sequence) 
          (foldr op initial (cdr sequence)))))

(define (filter pred ls)
  (foldr (lambda (x rest)
           (cond ((pred x) (cons x rest))
                 (else rest))) '() ls))

;test filter

(filter (lambda (x) (= x 1)) '(1 2 3 1 1 1))



(define (flatmap f ls)
  (foldr append '() (map f ls)))

; test flatmap

(flatmap (lambda (n) '(n n)) '(1 2))

; triple-orders

(define (enumerate-range n m)
  (cond ((> n m) '())
        (else (cons n (enumerate-range (+ n 1) m)))))

; test enumerate-range
(enumerate-range 1 5)

(define (triple-orders n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k) (list i j k))
                             (enumerate-range 1 n)))
                      (enumerate-range 1 n)))
           (enumerate-range 1 n)))

(define (sift-triple-orderes-by-sum n s)
  (filter (lambda (triple)
            (= s (foldr + 0 triple)))
          (triple-orders n)))

(sift-triple-orderes-by-sum 3 5)
