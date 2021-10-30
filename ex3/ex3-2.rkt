(load "utils.rkt")

(define (make-monitor f) 
  (let ((counter 0)) 
    (lambda (a . args)
      (cond ((and (= (length args) 0) (eq? a 'reset-counter))
             (set! counter 0))
            ((and (= (length args) 0) (eq? a 'how-many-calls?))
             counter)
            (else (begin (set! counter (+ counter 1))
                         (apply f (cons a args))))))))

(test "monitor"
  (= ((make-monitor sqrt) 4) 2)
  (= (let ((msqrt (make-monitor sqrt)))
      (msqrt 'how-many-calls?)) 0)
  (= (let ((msqrt (make-monitor sqrt)))
      (begin
        (msqrt 4)
        (msqrt 'how-many-calls?))) 1)
  (= (let ((msqrt (make-monitor sqrt)))
      (begin
        (msqrt 4)
        (msqrt 'reset-counter)
        (msqrt 'how-many-calls?))) 0)
)