; remove all the leading whitespaces of a string
(define (remove-leading-whitespaces ls)
  (let loop((lls ls))
             (cond
               ((null? lls) lls)
               ((char-whitespace? (car lls)) (loop (cdr lls)))
                   (else lls))))
; test remove-leading-whitespaces
(string=?
 (list->string (remove-leading-whitespaces (string->list "    hello")))
 "hello")
(string=?
 (list->string (remove-leading-whitespaces (string->list "    hello ")))
 "hello ")
(string=?
 (list->string (remove-leading-whitespaces (string->list "   ")))
 "")

; substring without leading whitespaces

(define (substring-no-leading-whitespaces str first last)
  (let ((ls (string->list (substring str first last))))
    (list->string (remove-leading-whitespaces ls))))

; test substring-no-leading-whitespaces
(string=? (substring-no-leading-whitespaces "  " 0 2) "")
(string=? (substring-no-leading-whitespaces "  hello world" 0 7) "hello")

; add-none-empty-word

(define (add-none-empty-word str ws)
  (cond ((= 0 (string-length str)) ws)
        (else (cons str ws))))

; test
(equal? (add-none-empty-word "" '("1")) '("1"))
(equal? (add-none-empty-word "2" '("1")) '("2" "1"))

; split into words
(define (words str)
  (reverse (let ((len (string-length str)))
    (let loop((first 0) (last 0) (ws '()))
      (cond ((= last len)
             (add-none-empty-word (substring-no-leading-whitespaces str first last) ws))
            ((char-whitespace? (string-ref str last))
             (let ((word (substring-no-leading-whitespaces str first last)))
               (cond ((= 0 (string-length word)) (loop last (+ last 1) ws))
                     (else (loop last (+ last 1) (cons word ws))))))
            (else (loop first (+ last 1) ws)))))))

; test
(words "1 2")
(words "  1 2  ")
