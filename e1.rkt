; Exercise 1.3
(define (square x) (* x x))
(define (sum-of-squares x y) (+ (square x) (square y)))
(define (larger-two-of-three x y z)
  (cond ((and (<= x y) (<= y z)) (list y z))
        (else (larger-two-of-three y z x))))
(define (sum-of-squares-of-larger-two x y z)
  (apply sum-of-squares (larger-two-of-three x y z)))

; test
(eqv? (sum-of-squares-of-larger-two 1 2 3) 13)
(eqv? (sum-of-squares-of-larger-two 1 1 1) 2)
(eqv? (sum-of-squares-of-larger-two 0 1 1) 2)

; Exercise 1.8
(define (newton-method target guess improve good-enough?)
  (letrec ((newton-method-rec (lambda (guess)
                               (let ((next-guess (improve target guess)))
                                 (cond ((good-enough? target guess next-guess) guess)
                                       (else (newton-method-rec next-guess)))))))
                             (newton-method-rec guess)))

(define (average x y) (/ (+ x y) 2.0))
(define (approximately-eqv? a b eps)
  (let ((rhs (* eps (cond ((< (abs a) (abs b)) (abs b))
                   (else (abs a))))))
    (<= (abs (- a b)) rhs)))
;test approximately-eqv?
(approximately-eqv? 1. 1. 0.01)
(not (approximately-eqv? 1. 0. 0.000001))
(define default-epsilon 0.00001)

;test newton-method for sqrt
(define (sqrt-improve target guess) (average guess (/ target guess)))
(define sqrt-step-epsilon default-epsilon)
(define (sqrt-step-detla-good-enough? target guess next-guess)
  (approximately-eqv? guess next-guess sqrt-step-epsilon))
(newton-method 9 3 sqrt-improve sqrt-step-detla-good-enough?)

;newton-method cude roots
(define (cube-improve target guess)
  (/ (+ (/ target (* guess guess)) (* 2 guess)) 3))
(define cube-step-epsilon default-epsilon)
(define (cube-step-detla-good-enough? target guess next-guess)
  (approximately-eqv? guess next-guess cube-step-epsilon))
(newton-method 27 3 cube-improve cube-step-detla-good-enough?)