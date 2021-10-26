(define (power-set-map ls)
  (map (lambda (n) (list '() (list n))) ls))
;test map

(power-set-map '(1 2 3))

(define (reduce begin acc ls)
  (let loop((b begin) (l ls))
    (cond ((null? l) b)
          (else (loop (acc b (car l)) (cdr l))))))

(define (mul-list f lls rls)
  (apply append (map (lambda (l) (map (lambda (r) (f l r)) rls)) lls)))

;test mul-list
(mul-list (lambda (l r) (+ l r)) '(1 2 3) '(1 2 3))


;test reduce
(reduce 0 (lambda (x y) (+ x y)) '(1 2 3))

(define (generate-subsets l r)
  (mul-list (lambda (ls rs) (append ls rs)) l r))

;test generate-subsets
(generate-subsets '(() (1)) '(() (2)))
(generate-subsets '(()) '(() (2)))

(define (power-set ls)
  (reduce '(())
          generate-subsets
          (power-set-map ls)))

(power-set '(1 2 3))