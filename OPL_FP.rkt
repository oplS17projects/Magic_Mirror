#lang racket ; insta auto calls servlet 
(require web-server/servlet
         web-server/servlet-env
        ; web-server/dispatch
         web-server/http
         ; web-server/templates
        ; 2htdp/image
         racket/date
        ; racket/promise
         )
(define list1 '(("city" . "Meredith")
  ("current" . 50.58)
  ("high" . 51.8)
  ("low" . 50)
  ("humidity" . 34)
  ("wind" . 26.410761154855646)
  ("description" . "Clouds")))
; functions
(define (finder word list)
 (cond ((null? list) "Nothing Found")
       ((and (pair? (car list))
            (string=? word (car(car list)))) (cdr(car list)))
       (else (finder word (cdr list)))))
(define (adder a b) (+ a b))
; test data 
(define city (finder "city" list1))
(define two 2)
(define number_list (list 1 2 3 4 5))
(define eight 8)
; What are some funcitons that would make my life easier?
; finder funciton so i can ask for say "current" and get back the current temp

; What HOF functions would come in handy? map, filter, foldr/foldl, accumulate, bucket
; I kind of made something similar to a  filter 

; How do I change the color of the text?
; How do I change the background to say blue?

; pass a string in, appends ".png" 
(define (pic_weather string)
  (string-append "cloudy" ".png"))

; displays newline in repl
(define (repl_newline)
  (display "\n"))
(display "\n");
(display (pic_weather (finder "city" list1)))
(display "\n")

;(number->string(finder "current" list1))
(date-display-format 'american)
 (define title1 "Magic Mirror");
(define (start req); start is a function that takes a request 
  (response/xexpr
   `(html (head (title "Magic Mirror")); this is the title of the webpage
          (body
           ;(left (h1 "I am above what you want to see."))(br)
           ;(center (h2 (string-append "test"  (number->string eight)))) ; should see an 8
           (left (h3 "City is: ",(finder "city" list1) (br)
                    "Humidity: ",(number->string(finder "humidity" list1)) (br)
                     "Current temp: " ,(number->string(finder "current" list1)) " Farenheit"(br) ; I want to see the number 50.58
                       ,(finder "description" list1))) (br)
            (left(img ([src "cloudy.png"]))); this outputs the picture
            
           ))))

(display "Believe\n")
         (serve/servlet start
#:listen-ip #f ; accept connections from external machines 
#:port 9000 ; change port number 
#:servlet-path "/MagicMirror"
#:extra-files-paths (list (build-path (current-directory) "images"))
#:launch-browser? #t) ; launches browser if true once you hit run 
