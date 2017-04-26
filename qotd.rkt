#lang racket
(require racket/trace)
(provide random-quote)
(define qfile (open-input-file "quotes.txt" ))

(define (quote-helper file)
   (let ((line (read-line file)))
     (if (eof-object? line)
     '()
     (cons line (quote-helper file)))))

(define quotes (quote-helper qfile))

(define (random-quote)
    (let  ((length- (+ (length quotes) 1)))
      (let ((rnd (- (random 1 length-) 1)))
          (list-ref quotes rnd))))