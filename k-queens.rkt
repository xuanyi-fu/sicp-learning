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

; k-queens

; empty-board
(define (empty-board board-size)
  (map (lambda (n) (list n)) (enumerate-interval 0 (- board-size 1))))

;safe?

;safe? : diagonal-rows

(define (upper-diagonal-rows row k)
  (enumerate-interval (- row k) (- row 1)))

(define (lower-diagonal-rows row k)
  (reverse (enumerate-interval (+ row 1) (+ row k))))


;safe? :  test diagonal-rows
;(equal? (diagonal-rows 2 1) '(0 1 2 3))

;any?

(define (any? ls)
  (foldr (lambda (x rest) (or x rest)) #f ls))

;define any-eqv?
(define (any-eqv? lls rls)
  (any? (map = lls rls)))

;test any-eqv?

;(not (any-eqv? '(1 2 3) '(4 5 6)))
;(any-eqv? '(1 2 3) '(4 5 3))

;(any? '(#f #t))
;(not (any? '(#f #f)))

;safe? : safe?

(define (safe? k positions)
  (let* ((last (list-ref positions k))
         (test-positions (reverse (cdr (reverse positions))))
         (upper-drows (upper-diagonal-rows last k))
         (lower-drows (lower-diagonal-rows last k)))
    (not (or (any-eqv? upper-drows test-positions)
         (any-eqv? lower-drows test-positions)
         (memv last test-positions)))))

; test safe

(safe? 2 '(2 6 1))
(not (safe? 2 '(3 6 1)))
(not (safe? 2 '(0 2 3)))

; adjoin-position

(define (adjoin-position new-row k rest-of-queens)
  (append rest-of-queens (list new-row)))

; queens
(define (queens board-size)
  (define (queens-cols k)
    (cond ((= k 0) (empty-board board-size))
          (else (let* ((prev (queens-cols (- k 1)))
                       (curr (filter
                              (lambda (positions) (safe? k positions))
                              (flatmap
                               (lambda (rest-of-queens)
                                 (map (lambda (new-row)
                                        (adjoin-position new-row k rest-of-queens))
                                      (enumerate-interval 0 (- board-size 1))))
                              prev))))
                  (begin curr)))))
  (queens-cols (- board-size 1)))

(length (queens 8))