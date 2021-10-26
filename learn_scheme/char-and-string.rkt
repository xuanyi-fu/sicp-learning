; Exercise 1
; first we define words
(define (words str) (let ((len (string-length str))
                          (wcons (lambda (first last ws)
                                   (cons (substring str first last) ws))))
                      (letrec((words-rec (lambda (first last ws)
                        (cond ((= first len) ws)
                              ((= last len) (wcons first last ws))
                              ((char-whitespace? (string-ref str last)) (words-rec (+ last 1) (+ last 1) (wcons first last ws)))
                              (else (words-rec first (+ last 1) ws))))))
          (reverse (words-rec 0 0 '())))))

; test words
(equal? (words "h s") '("h" "s"))
(equal? (words "hello world ") '("hello" "world"))
(equal? (words "") '())