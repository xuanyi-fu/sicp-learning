; first first not whitespace 
(define (finds-not-whitespace str start)
  (let ((len (string-length str)))
    (let loop((first start))
      (cond ((>= first len) len)
            ((char-whitespace? (string-ref str first)) (loop (+ first 1)))
            (else first)))))
; test

;(eqv? (finds-not-whitespace " " 0) 1)
;(eqv? (finds-not-whitespace "h w" 0) 0)
;(eqv? (finds-not-whitespace "h w" 1) 2)

; words
(define (words str)
  (let ((len (string-length str)))
    (letrec
        ((words-rec (lambda (first last ws)
                     (cond ((and (= first last) (= last len)) ws)
                           ((= first last) (words-rec first (+ last 1) ws))
                           ((char-whitespace? (string-ref str last))
                             (let ((new-first (finds-not-whitespace str first)))
                              (words-rec last last (cons (substring str new-first last) ws))))
                           (else (words-rec first (+ last 1) ws))))))
      (words-rec 0 0 '()))))
(words "h w")
