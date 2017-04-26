#lang racket 
(require web-server/servlet-env
         web-server/templates
         web-server/http
         xml
         racket/date )
(require (file "time.rkt"))
(require (file "weather.rkt"))
(require (file "preferences.rkt"))
(require (file "qotd.rkt"))

;; TODO: integrate files below 
;(require (file "wotd.rkt"))

; functions
(define (finder word list)
  (cond ((null? list) "Nothing Found")
        ((and (pair? (car list))
              (string=? word (car(car list)))) (cdr(car list)))
        (else (finder word (cdr list)))))

; pass a string in, appends ".png" 
(define (pic_weather string)
  (string-append string ".png"))

; gets current date from a date object 
(define (get-current-date arg)
  (car(cdr arg)))

; gets current time from a date object
(define (get-current-time arg)
  (cdr (cdr arg)))

(define city (cdr (assoc "datetime-city" (get-preferences))))
(define state (cdr (assoc "datetime-state" (get-preferences))))
(define dt (get-time city state))
(define location (string-append city ", " state))
                     
(define display-date-time (string-append "<script>var testDate = new Date(" (cdr dt) ");</script>"))
(define display-date-time-city (string-append "<script>$(document).ready(function() {$(\"#Location\").html(\"" location "\");});</script>"))

(define (start req)
  (response/xexpr
   `(html(head (title "Magic Mirror")); title of webpage; "`" is called a quasiqutoe
         (script ((src "clock.css")))
         (script ((src "https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js")))
         (body
          (div ,(make-cdata #f #f display-date-time))
          (div ,(make-cdata #f #f display-date-time-city))
          (script ((src "clock.js")))
          (div ,(make-cdata #f #f (include-template "clock-template.html")))          
          )))) 

(serve/servlet start
               #:listen-ip #f ; accept connections from external machines 
               #:port 9000 ; change port number 
               #:servlet-path "/MagicMirror"
               #:extra-files-paths (list (build-path (current-directory) "images")
                                         (current-directory))
               #:launch-browser? #t) ; launches browser if true once you hit run 
