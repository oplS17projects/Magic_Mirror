# Magic Mirror

## Steve Kim 
### April 30, 2017

# Overview
I worked with Steve Warren (@LordSpaghettiOs) to make a magic mirror (smart mirror).  

**Authorship note:** All of the code described here was written by myself unless otherwise specified.

# Libraries Used
The mirror required ten libraries.  Each module, such as the weather, was containted in its own Racket file which concealed its implmentation.     
```Racket
(require web-server/servlet-env
         web-server/templates
         web-server/http
         xml)
(require (file "time.rkt"))
(require (file "weather.rkt"))
(require (file "preferences.rkt"))
(require (file "wotd.rkt"))
(require (file "qotd.rkt"))
```
The ```web-server/servlet``` library provided server which is the heart of the mirror. 
The ```web-server/http``` http is used withint our servlet.
The ```web-server/templates``` library allowed us to use existing css and html templates to arrange the modules in our mirror. 
The ```xml``` library allowed for parsing and generating xml code for customizing our modules. This library had a function ```(make-cdata) ``` that was used heavily in the servlet.  Every module uses this function to have character data.

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
It is then called in the severlet this way.  
```Racket
(div , (make-cdata #f #f get-quote))
```
## 3. Higher Order Functions (HOF)
The finder function notated in the recursion section is the best sample of a HOF since it operates on a list with elements being pairs.  Its behavior is analogous to filter returning what I was searching for.  This function was used heavily in developing and displaying weather information.  This was all well part of the abstraction barrier since the structure of the list was communicated previously.    

## 4.  Functional Approach To Processing Data
```Racket
(define get-quote (string-append "<script>$(document).ready(function() {$(\"#quote\").html(\"" (random-quote) "\");});</script>"))
(define (start req)
  (response/xexpr
   `(html(head
          (title "Magic Mirror"); title of webpage; "`" is called a quasiqutoe
          (script ((src "https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js")))
          ,(make-cdata #f #f font-include))
         
         (body
          ; current weather
          (div , (make-cdata #f #f weather-current-icon))
          (div , (make-cdata #f #f weather-current-loc))
          (div , (make-cdata #f #f weather-current-current))
          (div , (make-cdata #f #f weather-current-humidity))
          (div , (make-cdata #f #f weather-current-desc))

          ; forecast weather
          (div , (make-cdata #f #f five-day-forecast))

          ; qotd
          (div , (make-cdata #f #f get-quote))
          
          ; date/time
          (div ,(make-cdata #f #f display-date-time))
          (div ,(make-cdata #f #f display-date-time-city))
          (script ((src "clock.js")))
          
          ; word of the day
          (div ,(make-cdata #f #f word-of-the-day))
          (div ,(make-cdata #f #f defs-of-the-day))
          
          (script ((src "wotd.js")))

          ; template
          (div ,(make-cdata #f #f (include-template "layout.html"))))))) 
``` 
          
The above code is the heart of the mirror.  Each display component has a div tag associated with it.
The function make-cdta allowed for defined functioned outputs to be displayed using templates. The above example shows how a 
random quote  was generated.  The ```(random-quote)```  returnts a strings which gets appended to a script.  This script is 
bound to ```get-quote```  which it contained in ```(div, (make-cdata #f #F get-quote))```.  The webpage itself is an 
occurance of the above function.  

## External Technology
Our group was one of the few groups who utalized external technology in our project.  The mirror itself is composed of a HDTV 
and a piece of reflective plastic.  Sadly at the I2I convention the powerful lights reduced the visability of the mirror.  
The mirror itself was a great success.  
