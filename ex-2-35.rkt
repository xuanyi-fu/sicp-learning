(define (accumulate op initial sequence) 
  (if (null? sequence) 
      initial 
      (op (car sequence) 
          (accumulate op initial (cdr sequence)))))

(define (count-leaves t)
  (accumulate (lambda (tree res)
                (+ (cond ((not (list? tree)) 1)
                         (else (count-leaves tree)))
                   res))
              0 t))

(count-leaves '(((1) (2) (3)) (1 2 3)))