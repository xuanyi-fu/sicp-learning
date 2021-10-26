;Exercise 4
; 4.1
(define (my-reverse ls)
  (letrec ((reverse-rec (lambda (lls rls)
                          (cond ((null? lls) rls)
                                (else (reverse-rec (cdr lls) (cons (car lls) rls)))))))
          (reverse-rec ls '())))

; test 4.1
(equal? (my-reverse '()) '())
(equal? (my-reverse '(1)) '(1))
(equal? (my-reverse '(1 2 3)) '(3 2 1))

; 4.2
(define (my-sum ls)
  (letrec
      ((sum-rec (lambda (l s)
                 (cond ((null? l) s)
                       (else (sum-rec (cdr l) (+ (car l) s)))
                       ))))
      (sum-rec ls 0)))

; test 4.2
(eqv? (my-sum '()) 0)
(eqv? (my-sum '(1)) 1)
(eqv? (my-sum '(1 2 3)) 6)

;4.3
(define (string-to-int str)
  (letrec ((number-list (map (lambda (n) (- (char->integer n) 48)) (string->list str)))
           (stoi-rec (lambda (ls n)
                       (cond ((null? ls) n)
                             (else (stoi-rec (cdr ls) (+ (car ls) (* n 10))))))))
    (stoi-rec number-list 0)))

;test 4.3
(eqv? (string-to-int "") 0)
(eqv? (string-to-int "1") 1)
(eqv? (string-to-int "1234") 1234)
