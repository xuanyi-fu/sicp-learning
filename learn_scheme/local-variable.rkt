(define (flying-distance v theta)
  (let* (
    ; local variables
    (pi (* 4 (atan 1.0)))
    (rad (* (/ theta 180) pi))
    (g 9.8)
    (vy (* v (cos rad)))
    (vx (* v (sin rad)))
    (time-reach-ground (/ (* 2 vy) g)))
    ; body
    (* vx time-reach-ground)
  ))

(flying-distance 40 30)