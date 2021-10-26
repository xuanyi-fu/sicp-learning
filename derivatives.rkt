(load "utils.rkt")

; number utils
(define (is-0 exp)
  (and (number? exp) (= exp 0)))

(define (is-1 exp)
  (and (number? exp) (= exp 1)))

;test

(and (is-0 0)
(not (is-0 'a))
(is-1 1)
(not (is-1 0)))

; variable
(define variable? symbol?)
(define (same-variable? lhs rhs)
  (and (variable? lhs) (variable? rhs) (eq? lhs rhs)))

; test same-variable

(and
 (not (same-variable? 'a 'b))
 (same-variable? 'a 'a)
 (not (same-variable? '() 'a))
 (not (same-variable? '(a) '())))

; sum

(define (make-sum-2 lhs rhs)
  (cond ((is-0 lhs) rhs)
        ((is-0 rhs) lhs)
        ((and (number? lhs) (number? rhs)) (+ lhs rhs))
        (else
         (list '+ lhs rhs))))

(define (make-sum . w)
  (foldr make-sum-2 0 w))

; sum-lhs

(define sum-lhs cadr)
(define (sum-rhs exp)
  (and (sum? exp) (apply make-sum (cddr exp))))

; check first symbol

(define (first-symbol-eq? exp s)
  (let ((first-symbol (car exp)))
    (and (symbol? first-symbol)
         (eq? first-symbol s))))

; sum?
(define (sum? exp)
  (and (list? exp) (first-symbol-eq? exp '+)))

; test sum


(test "sum"
  (same-variable? (sum-lhs (make-sum 'a 'b)) 'a)
  (same-variable? (sum-rhs (make-sum 'a 'b)) 'b)
  (sum? (make-sum 3 'a))
  (sum? (make-sum 1 'a))
  (same-variable? (make-sum 'a 0) 'a)
  (same-variable? (make-sum 0 'a) 'a)
  (= (make-sum 0 0) 0)
  (not (sum? (make-sum 1 2)))
  (equal? (sum-rhs '(+ 1 2 x)) '(+ 2 x)))



; mul

(define (make-mul-2 lhs rhs)
  (cond ((or (is-0 lhs) (is-0 rhs))
         0)
        ((and (number? lhs) (number? rhs))
         (* lhs rhs))
        ((is-1 lhs)
         rhs)
        ((is-1 rhs)
         lhs)
        (else
         (list '* lhs rhs))))

(define (make-mul . w)
  (foldr make-mul-2 1 w))

(define (mul? exp)
  (and (list? exp) (first-symbol-eq? exp '*)))

(define mul-lhs cadr)
(define (mul-rhs exp) (and (mul? exp) (apply make-mul (cddr exp))))

; test mul
(test "mul"
 (same-variable? (mul-lhs (make-mul 'a 'b)) 'a)
 (same-variable? (mul-rhs (make-mul 'a 'b)) 'b)
 (mul? (make-mul 3 'a))
 (not (mul? (make-mul 1 'a)))
 (same-variable? (make-mul 'a 1) 'a)
 (same-variable? (make-mul 1 'a) 'a)
 (= (make-mul 0 'a) 0)
 (= (make-mul 'a 0) 0)
 (not (mul? '(+ 1 2)))
 (equal? (mul-rhs '(* 1 2 a)) '(* 2 a)))


; expn
(define (make-expn lhs rhs)
  (cond ((number? rhs)
         (cond ((= rhs 1) lhs)
               ((= rhs 0) 1)))
        (else "error rhs is not a number")))

; dc/dx = 0
; dx/dx = 1
; d(u + v)/dx = du/dx + dv/dx
; d(uv)/dx = u dv/dx + v du/dx

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (cond ((same-variable? exp var) 1)
                (else exp)))
        ((sum? exp)
         (make-sum (deriv (sum-lhs exp) var)
                   (deriv (sum-rhs exp) var)))
        ((mul? exp)
         (make-sum (make-mul (mul-lhs exp) (deriv (mul-rhs exp) var))
                   (make-mul (mul-rhs exp) (deriv (mul-lhs exp) var))))
        (else "unrecognized expression")))

(test "deriv" 
(deriv '(+ x x x) 'x)
(deriv '(* x x x) 'x)
)

