; utilities
(define (foldr op initial sequence) 
  (if (null? sequence) 
      initial 
      (op (car sequence) 
          (foldr op initial (cdr sequence)))))

(define (filter pred ls)
  (foldr (lambda (x rest)
           (cond ((pred x) (cons x rest))
                 (else rest))) '() ls))

(define (flatmap f ls)
  (foldr append '() (map f ls)))

(define (enumerate-interval n m)
  (cond ((> n m) '())
        (else (cons n (enumerate-interval (+ n 1) m)))))

;filter-map
(define (filter-map pred f ls)
  (foldr (lambda (x rest)
           (cond ((pred x) (cons (f x) rest))
                 (else rest))) '() ls))

; test filter-map

(equal? (filter-map (lambda (x) (= x 2))
                    (lambda (x) (* x 100))
                    '(1 2 2 3 4))
        '(200 200))

(equal? (filter-map (lambda (x) (= x 2))
                    (lambda (x) (* x 100))
                    '(1 3 4))
        '())

(equal? (filter-map (lambda (x) (= x 2))
                    (lambda (x) (* x 100))
                    '())
        '())

; any 

(define (any pred ls)
  (foldr (lambda (x rest)
           (cond ((pred x) #t)
                 (else (or #f rest)))) #f ls))

;test any

(any (lambda (x) (= x 2)) '(1 2 3))

;any?

(define (any? ls)
  (foldr (lambda (x rest) (or x rest)) #f ls))

; safe?

(define (safe? row positions)
  (let* ((position-length (length positions))
         (distances (reverse (enumerate-interval 1 position-length)))
         (distance-to-row (map (lambda (x) (abs (- x row))) positions)))
     (not (or (any? (map = distances distance-to-row))
              (memv row positions)))))

; test safe
(safe? 1 '(2 6))
(not (safe? 1 '(3 6)))
(not (safe? 3 '(0 2)))

; initial-board

(define (initial-board board-size)
  (map list (enumerate-interval 0 (- board-size 1))))

; test initial-board

(equal? (initial-board 1) '((0)))
(equal? (initial-board 2) '((0) (1)))

; adjoin-position

(define (adjoin-position row positions)
  (append positions (list row)))

; k-queens
(define (k-queens board-size)
  (define max-column-index (- board-size 1))
  (define (k-queens-rec k)
    (cond ((= k 0) (initial-board board-size))
          (else (flatmap (lambda (positions)
                           (filter-map (lambda (new-row) (safe? new-row positions))
                                       (lambda (new-row) (adjoin-position new-row positions))
                                       (enumerate-interval 0 max-column-index)))
                         (k-queens-rec (- k 1))))))
  (k-queens-rec max-column-index))
; test k-queens
(eqv? (length (k-queens 5)) 10)
(eqv? (length (k-queens 8)) 92)