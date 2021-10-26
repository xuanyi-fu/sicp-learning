(define my-length
  (lambda (a-list)
   (if (null? a-list)
       0
       (+ 1 (my-length (cdr a-list)))
   )
  )
)

(define add1 (lambda (a-number) (+ 1 a-number)))
(define (sub1 a-number) (- a-number 1))

(define pi (* 4 (atan 1.0)))
(define (deg2rad a-deg) (* (/ a-deg 180) pi))
(define (distance-with-v-t v t) (* v t))
(define g 9.8)
(define (time-reach-ground vy) (/ (* 2 vy) g))
(define (vx v theta) (* v (cos theta)))
(define (vy v theta) (* v (sin theta)))
(define (flying-distance v theta)
  (*
   (vx v (deg2rad theta))
   (time-reach-ground (vy v (deg2rad theta)))))
(flying-distance 40 30)
