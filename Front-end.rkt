#lang racket 
(require web-server/servlet-env
         web-server/templates
         web-server/http
         xml
         racket/date )
(require (file "time.rkt"))
(require (file "weather.rkt"))
(require (file "preferences.rkt"))
(require (file "wotd.rkt"))


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

(define weather-current (weather "current"
                                 (cdr (assoc "weather-city" (get-preferences)))
                                 (cdr (assoc "weather-state" (get-preferences)))))

(define weather-forecast (weather "forecast"
                                 (cdr (assoc "weather-city" (get-preferences)))
                                 (cdr (assoc "weather-state" (get-preferences)))))

(define city (cdr (assoc "datetime-city" (get-preferences))))
(define state (cdr (assoc "datetime-state" (get-preferences))))
(define dt (get-time city state))
(define location (string-append city ", " state))                 

(define (get-definitions items defs)
  (if (null? items) defs
      (get-definitions (cdr items) (string-append defs "<h6>" (car items) "</h6>"))))

(define defs (get-definitions (cdr (wotd)) ""))

(define font-include "<link href=\"https://fonts.googleapis.com/css?family=Josefin+Sans\" rel=\"stylesheet\">")

(define display-date-time (string-append "<script>var testDate = new Date(" (cdr dt) ");</script>"))
(define display-date-time-city (string-append "<script>$(document).ready(function() {$(\"#Location\").html(\"" location "\");});</script>"))
(define word-of-the-day (string-append "<script>$(document).ready(function() {$(\"#setword\").html(\"" (car (wotd)) "\");});</script>"))
(define defs-of-the-day (string-append "<script>$(document).ready(function() {$(\"#setdefs\").html(\"" defs "\");});</script>"))


(define weather-current-loc (string-append "<script>$(document).ready(function() {$(\"#loc\").html(\""
                                           (cdr (assoc "city" weather-current)) ", "
                                           (cdr (assoc "weather-state" (get-preferences))) "\");});</script>"))

(define weather-current-current (string-append "<script>$(document).ready(function() {$(\"#current-temp\").html(\"" (number->string (inexact->exact (round (cdr (assoc "current" weather-current))))) "&#176; F\");});</script>"))
(define weather-current-humidity (string-append "<script>$(document).ready(function() {$(\"#humidity\").html(\"" (number->string (cdr (assoc "humidity" weather-current))) "% Humidity\");});</script>"))
(define weather-current-desc (string-append "<script>$(document).ready(function() {$(\"#description\").html(\"" (cdr (assoc "description" weather-current)) "\");});</script>"))
(define weather-current-icon (string-append "<script>$(function(){$(\"#icon\").html('<img id=\"current-icon\" src=\"" (cdr (assoc "icon" weather-current)) ".png\" alt=\"icon\" />');});</script>"))   

; new stuff here
(define (generate-forecast count items script-string)
  (if (> count 5) script-string
      (begin
        (let ([js(string-append "<script>$(function(){$(\"#d" (number->string count) "_icon\").html('<img id=\"d" (number->string count) "_icon\" src=\""
                                (cdr (assoc "icon" (cdr (car items)))) ".png\" alt=\"icon" (number->string count) "\" />');});</script>" script-string)])
          
          (let ([top-row (string-append "<script>$(document).ready(function() {$(\"#d" (number->string count) "_toprow\").html(\""
                                        (cdr (assoc "day-of-week" (cdr (assoc "date" (cdr (car items)))))) " "
                                        (number->string (cdr (assoc "month" (cdr (assoc "date" (cdr (car items))))))) "/"
                                        (number->string (cdr (assoc "day" (cdr (assoc "date" (cdr (car items))))))) "\");});</script>")])
            
            (let ([bottom-row (string-append "<script>$(document).ready(function() {$(\"#d" (number->string count) "_bottomrow\").html(\""
                                             (cdr (assoc "description" (cdr (car items)))) " "
                                             (number->string (inexact->exact (round (cdr (assoc "high" (cdr (car items))))))) "&#176; F / "
                                             (number->string (inexact->exact (round (cdr (assoc "low" (cdr (car items))))))) "&#176; F\");});</script>")])

              (generate-forecast (+ count 1) (cdr items) (string-append js top-row bottom-row))))))))

(define five-day-forecast (generate-forecast 1 weather-forecast ""))

;<div id="d1_icon"></div>
          ;<div id="d1_date"></div>
          ;<div id="d1_desc"></div>
          ;<div id="d1_hilo"></div>

; nighttime icons start at 9pm
; daytime icons start at 6am

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

(serve/servlet start
               #:listen-ip #f ; accept connections from external machines 
               #:port 9000 ; change port number 
               #:servlet-path "/MagicMirror"
               #:extra-files-paths (list (build-path (current-directory) "images")
                                         (current-directory))
               #:launch-browser? #t) ; launches browser if true once you hit run 
