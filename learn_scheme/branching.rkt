; Exercise 1
; 1.

(define (my-abs a-number)
  (if (< a-number 0) (- 0 a-number) a-number))

; 1. tests
(= (my-abs 0) 0)
(= (my-abs 1) 1)
(= (my-abs -1) 1)

; 2. reciprocal

(define (my-reciprocal a-number)
  (if (= a-number 0) #f (/ 1 a-number)))

; 2. tests
(= (my-reciprocal (/ 1 5)) 5)
(not (my-reciprocal 0))

; 3. integer to char
(define (my-integer-to-char a-integer)
  (if (and (>= a-integer 33) (<= a-integer 126))
      (integer->char a-integer)
      #f))
; 3. tests
(eq? (my-integer-to-char 33) #\!)
(not (my-integer-to-char 30))
