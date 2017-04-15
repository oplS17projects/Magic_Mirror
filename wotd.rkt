#lang racket
(require net/url)

(provide wotd)

; the regexp return type is a list of bytes
; bytes to string is done with bytes->string/utf-8

; TODO
; protect against network failure
; cache page to keep from spamming

(define (wotd)
  (def 1 (list (word))))

(define (word)
  (let* ([url "http://www.dictionary.com/wordoftheday/"]
         [rx #rx"(?<=<title>Get the Word of the Day - ).*?(?= | Dictionary.com</title>)"])
    (bytes->string/utf-8 (car (regexp-match rx (get-pure-port (string->url url)))))))

(define kv (list (cons 1 "first") (cons 2 "second") (cons 3 "third") (cons 4 "fourth") (cons 5 "fifth")))

(define (def num items)
  (let ([url "http://www.dictionary.com/wordoftheday/"]
        [rx (string-append "(?<=<li class=\"" (cdr (assoc num kv)) "\"><span>).*?(?=</span>)")])
    (define result (regexp-match rx (get-pure-port (string->url url))))
    
   (if (or (false? result) (> num 5) (< num 1)) (reverse items)
        (def (+ num 1) (cons (string-append (number->string num) ": " (bytes->string/utf-8 (car result))) items)))))
         
