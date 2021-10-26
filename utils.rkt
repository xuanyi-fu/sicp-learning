;utils
(define (foldl op initial sequence)
  (let loop((result initial) (seq sequence))
        (cond ((null? seq) result)
              (else loop( (op (car seq) result) (cdr seq))))))

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

; test framekwork

(define show
  (lambda args
    (for-each display args)
    (newline)))

(define (test name . tests)
  (begin 
    (show "--- BEGIN tests for " name " ---")
    (for-each (lambda (t idx) (let ((result t)) 
                                    (cond ((not (eqv? result #t)) (show idx ": " t)))))
      tests
      (enumerate-interval 1 (length tests))
    )
    (show "--- END tests for " name " ---")))

; (define (list-and list)
;   (if (null? list)
;     #t
;     (and (car list))))

; (define (test-with-cases function-to-test test-cases)
;   (let*
;     (
;       (test-statuses
;         (map
;           (lambda (test-case) (test-with-case function-to-test test-case))
;           test-cases))
;       (all-tests-passed (list-and test-statuses))
;     )
;     (if all-tests-passed
;       (display-all-on-line "All tests for " function-to-test " passed."))))

; (define (test-with-case function-to-test test-case)
;   (let*
;     (
;       (input-args (first test-case))
;       (desired-output (second test-case))
;       (actual-output (apply function-to-test input-args))
;     )
;     (if (equal? desired-output actual-output)
;       #t
;       (begin
;         (display-all-on-line function-to-test " failure for inputs " input-args " - expected " desired-output ", got " actual-output)
;         #f))))       