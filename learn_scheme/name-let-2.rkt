;Exercise 3
; named let for tail recursion
; 2.1

(define (my-reverse ls)
  (let loop((lls ls) (rls '()))
    (cond ((null? lls) rls)
          (else (loop (cdr lls) (cons (car lls) rls))))))

; test 2.1
(equal? (my-reverse '()) '())
(equal? (my-reverse '(1)) '(1))
(equal? (my-reverse '(1 2 3)) '(3 2 1))

; 2.2
(define (my-sum ls)
  (let loop((lls ls) (sum 0))
    (cond ((null? lls) sum)
          (else (loop (cdr lls) (+ (car lls) sum))))))

; test 2.2
(eqv? (my-sum '()) 0)
(eqv? (my-sum '(1)) 1)
(eqv? (my-sum '(1 2 3)) 6)

; 2.3
(define (string-to-int str)
  (let* ((char-list (string->list str))
         (number-list (map (lambda (c) (- (char->integer c) 48)) char-list)))
    (let loop((lls number-list) (number 0))
      (cond ((null? lls) number)
            (else (loop (cdr lls) (+ (* number 10) (car lls))))))))

; test 2.3
(eqv? (string-to-int "") 0)
(eqv? (string-to-int "1") 1)
(eqv? (string-to-int "1234") 1234)

; 3 range
(define (range n)
  (let loop((m 0) (ls '()))
    (cond ((= m n) ls)
          (else (loop (+ m 1) (cons (- (- n m) 1) ls))))))
; test 3
(equal? (range 0) '())
(equal? (range 1) '(0))
(equal? (range 4) '(0 1 2 3))