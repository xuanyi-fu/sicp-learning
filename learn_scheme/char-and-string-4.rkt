; span
(define (span pred ls)
  (cond ((null? ls) (list '() '()))
        (else
         (cond ((pred (car ls))
                (let* ((pair (span pred (cdr ls)))
                       (lls (car pair))
                       (rls (car (cdr pair))))
                  (list (cons (car ls) lls) rls)))
               (else (list '() ls))))))

; span2
(define (span2 pred ls)
  (let loop((lls '()) (rls ls))
    (cond ((and (not (null? rls)) (pred (car rls)))
           (loop (cons (car rls) lls) (cdr rls)))
          (else (list (reverse lls) rls)))))

; test
(span (lambda (n) (< n 3)) '(1 2 3 4))
(equal? (span (lambda (n) (< n 3)) '(1 2 3 4))
        '((1 2) (3 4)))
(equal? (span (lambda (n) (< n 3)) '(1 2))
        '((1 2) ()))
(equal? (span (lambda (n) (< n 3)) '())
        '(() ()))
(equal? (span (lambda (n) (< n 3)) '(4 4 4))
        '(() (4 4 4)))

(equal? (span (lambda (n) (= n 1)) '(2 3 1 2 3))
        '((2 3) (1 2 3)))
; test span2
(equal? (span2 (lambda (n) (< n 3)) '(1 2 3 4))
        '((1 2) (3 4)))
(equal? (span2 (lambda (n) (< n 3)) '(1 2))
        '((1 2) ()))
(equal? (span2 (lambda (n) (< n 3)) '())
        '(() ()))
(equal? (span2 (lambda (n) (< n 3)) '(4 4 4))
        '(() (4 4 4)))

(cons (cons 1 '(1 2)) (cdr '((1 2) (3 4))))
(list (cons 1 '(1 2)) (cdr '((1 2) (3 4))))
(cdr '((1 2) (3 4)))