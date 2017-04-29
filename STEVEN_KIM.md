
# Magic Mirror

## Steve Kim 
### April 30, 2017

# Overview
I worked with Steve Warren (@LordSpaghettiOs) to make a magic mirror (smart mirror).  

**Authorship note:** All of the code described here was written by myself unless otherwise specified.

# Libraries Used
```Racket
(require web-server/servlet-env
         web-server/templates
         web-server/http
         xml
         racket/date )
(require (file "time.rkt"))
(require (file "weather.rkt"))
(require (file "preferences.rkt"))
(require (file "wotd.rkt"))
(require (file "qotd.rkt"))
```
# Key Code Excerpts
Here is a discussion of the most essential procedures, including a description of how they embody ideas from UMass Lowell's COMP.3010 Organization of Programming languages course.

## 1. Recursion
  
```racket
(define (quote-helper file)
   (let ((line (read-line file)))
     (if (eof-object? line)
     '()
     (cons line (quote-helper file)))))
```
 The quote- helper takes a text file as its single parameter.  It and will continue reading  a line of text and accumulating the lines of codes into a list.  This will continue as long as the line is not an eof (end of file)-object.  
```racket
(define (finder word list)
  (cond ((null? list) "Nothing Found")
        ((and (pair? (car list))
              (string=? word (car(car list)))) (cdr(car list)))
(else (finder word (cdr list)))))
```
The finder function takes in two parameters a string for the word being searched and a list.  It will first check if the list is null  which provides both a terminating case as well if the list is empty.  It will then ensure that the entries are tuple(pairs) and that the first string (car of the entry) matches the word.  If both are the same it will return the second entry of the pair.  If this condition is not met it will recurse. 

## 2. Data Abstraction
```racket
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
    (let*  ((length- (+ (length quotes) 1)))
      (let* ((rnd (- (random 1 length-) 1)))
         (string-normalize-spaces (list-ref quotes rnd)))))
```
The above is the implementation of quote of the day (qotd) which is kept in a separate file.  This below line allows for only one function to be used in any program that uses this file. 
```racket 
(provide random-quote)
```
If in the future I were to change how I were to implement how a random quote got generated.  I could make that change in one file and not in multiple spots.  It well obscures any helper functions that were created and used in the process.  

In addition any web scripting code simply called a Racket function which would return or be converted to a string.  The below is an example of how the qotd would get displayed in the servlet. 
```racket
(define get-quote (string-append "<script>$(document).ready(function() {$(\"#quote\").html(\"" (random-quote) "\");});</script>"))
```

## 4. External Technology
