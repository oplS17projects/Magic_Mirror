#lang racket

(define qfile (open-input-file "/Users/Sunhee/documents/Magic_Mirror/quotes.txt" ))


 (define (quote-helper file)
   (let ((line (read-line file)))
     (if (eof-object? line)
     '()
     (cons line (quote-helper file)))))

(define (random-quote)
  (define source (quote-helper qfile))
(define mod (length source))

(list-ref source (remainder (arithmetic-shift (random 1 200) 10) mod)))