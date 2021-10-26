; span from char-and-string-4.rkt
(define (span pred ls)
  (let loop((lls '()) (rls ls))
    (cond ((and (not (null? rls)) (pred (car rls)))
           (loop (cons (car rls) lls) (cdr rls)))
          (else (list (reverse lls) rls)))))

; spans
(define (spans pred ls)
  (cond ((null? ls) '())
        (else (let* ((pair (span pred ls))
                     (lspan (car pair))
                     (rspan (car (cdr pair))))
                (cons lspan (spans (lambda (n) (not (pred n))) rspan))))))

; filter
(define (filter pred ls)
  (let loop((lls ls) (rls '()))
    (cond ((null? lls) rls)
          ((pred (car lls)) (loop (cdr lls) rls))
          (else (loop (cdr lls) (cons (car lls) rls))))))

; test filter
(filter (lambda (n) (< n 4)) '(1 2 3 4))

; test spans

;(equal? (spans (lambda (n) (= n 1)) '(1 1 2 3 1 1 1 2 3))
;        '((1 1) (2 3) (1 1 1) (2 3)))
;(equal? (spans (lambda (n) (= n 1)) '(2 4 4)) '(() (2 4 4)))

(filter (lambda (ls) (or (= 0 (length ls)) (char-whitespace? (car ls))))
        (reverse (spans (lambda (c) (char=? #\Space c)) (string->list "hello world!"))))