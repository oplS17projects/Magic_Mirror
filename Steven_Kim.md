
# Magic Mirror

## Steve Kim 
### April 30, 2017

# Overview


**Authorship note:** All of the code described here was written by myself unless otherwise specified.

# Libraries Used


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
```racket
(define (finder word list)
  (cond ((null? list) "Nothing Found")
        ((and (pair? (car list))
              (string=? word (car(car list)))) (cdr(car list)))
(else (finder word (cdr list)))))
```
## 2. Data Abstraction
## 3. Higher Order Functions (HOF)
## 4. External Technology
