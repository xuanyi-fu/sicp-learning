(load "utils.rkt")

(define (make-accumulator init) 
  (lambda (acc) 
    (begin 
      (set! init (+ init acc))
      init)))

(test "accumulator"
  (= ((make-accumulator 10) 0) 10)
  (= ((make-accumulator 20) 10) 30)
)


