(define (add-to-subsets n lls)
  (map (lambda (ls) (cons n ls)) lls))

;test add-to-subsets
(add-to-subsets 1 '(() (2)))

(define (powerset s)
  (cond ((null? s) '(()))
        (else (let ((rest (powerset (cdr s))))
                (append rest
                        (add-to-subsets (car s) rest))))))

(powerset '(1 2 3 4))