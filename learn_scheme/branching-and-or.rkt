; Exercise 2
; 1.
(define (three-product a b c) (and (> a 0) (> b 0) (> c 0) (* a b c)))
; 1. tests
(= (three-product 1 1 1) 1)
(not (three-product 1 0 1))

; 2.
(define (three-product-2 a b c)
  (and (or (< a 0) (< b 0) (< c 0))
       (* a b c)))
; 2. tests
(= (three-product-2 -1 1 1) -1)
(= (three-product-2 -1 -1 1) 1)
(= (three-product-2 -1 -1 -1) -1)
(not (three-product-2 1 1 1))