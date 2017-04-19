#lang racket ; insta auto calls servlet 
(require web-server/servlet
         web-server/servlet-env
         web-server/http
         racket/date
         (file "weather.rkt")
         )
(require (file "time.rkt"))
(require (file "wotd.rkt"))
(require web-server/templates)
(require (file "preferences.rkt"))
(require xml)
;(include-template "test.html")
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

;(number->string(finder "current" list1))
(date-display-format 'american)
 (define title1 "Magic Mirror Dynamic");
; doesn't work on windows 10 unless you run racket as an adminstrator, it needs the permission to create
; required folder structure 
(define (start req) 
  (response/xexpr
   `(html(head (title "Magic Mirror")); title of webpage; "`" is called a quasiqutoe 
         (body
          ;(left (h1 "Current Date: ",(get-current-date Dayton-time)))(br) ; this updates automatically
          ;(left (h1 "Current time: ", (get-current-time Dayton-time))) (br) ; this updates automatically
          ;(left (h2 , (finder "city" Dayton-OH))(br)
                ;(h2 "Current temperature: ",(number->string (finder "current" Dayton-OH))))(br) ; this updates automatically
         ;       (left (img ([src ,(pic_weather (finder "description" Dayton-OH))])))                                                               
         ; (h3, (car(cdr(wotd))))
         ;(script ((src "http://ajax.google.com/ajax/libs/jquery/3.2.1/jquery.min.js")))
       ; (include-template "test.html")
       )
     ;(h3  ,(make-cdata #f #f(include-template "test.html")))
         )))
  
         (serve/servlet start
#:listen-ip #f ; accept connections from external machines 
#:port 9000 ; change port number 
#:servlet-path "/MagicMirror"
#:extra-files-paths (list (build-path (current-directory) "images"))
#:launch-browser? #t) ; launches browser if true once you hit run 
