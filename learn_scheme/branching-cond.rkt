; this tests the order of executing the clauses.
(define my-number 1)
(define (test-cond a-number)
  (cond ((> a-number 0) (set! my-number 2) (set! my-number (+ my-number 1)))))

; Exercise 3
(define score-grade-map '(("A" (80 100))
                    ("B" (60 79))
                    ("C" (40 59))
                    ("D" (0 40))))
(define (upper-bound p) (car (cdr (car (cdr p)))))
(define (lower-bound p) (car (car (cdr p))))
(define (grade p) (car p))
; test upper-bound, lower-bound, and grade
(= (upper-bound '("A" (80 100))) 100)
(= (lower-bound '("A" (80 100))) 80)
(eqv? (grade '("A" (80 100))) "A")

(define (falls-within score p) (and (<= score (upper-bound p)) (>= score (lower-bound p) ) p))
; test falls-within
(not (not (falls-within 80 '("A" (80 100)))))
(not (falls-within 79 '("A" (80 100))))

(define (first-not-false a-list)
  (cond
    ((null? a-list) '())
    ((car a-list) (car a-list))
    (else (first-not-false (cdr a-list)))))
; test first-not-false
(eqv? (car (first-not-false '(#f ("B" (60 79)) ("C" (40 59)) ("D" (0 40))))) "B")

(define (grade score) (car (first-not-false
    (map (lambda (p) (falls-within score p)) score-grade-map))))
; test grade
; I do not know how to define a local symbol in a function now
; these tests cannot be achieved
;(grade 101)
;(grade -1)

(eqv? (grade 80) "A")
(eqv? (grade 79) "B")
(eqv? (grade 59) "C")
(eqv? (grade 30) "D")
