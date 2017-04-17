#lang racket
(require net/url json gregor)

(require (file "api-keys.rkt"))
(provide weather)

(define current-url "http://api.openweathermap.org/data/2.5/weather?q=")
(define forecast-url "http://api.openweathermap.org/data/2.5/forecast/daily?q=")
(define options "&mode=json&units=imperial&cnt=7")
(define max-cache-age-hours 1) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (weather stream-type city state-country)
  (make-dir "weather_data")
  (make-dir "weather_data/current")
  (make-dir "weather_data/forecast")

  (cond
    [(string=? stream-type "current") (get-weather current-url stream-type city state-country)]
    [(string=? stream-type "forecast") (get-weather forecast-url stream-type city state-country)]
    [else null]
  ))

(define (network-failure)
  (λ _ (printf "Network Unavailable\n")))

;;(define (no-network-or-cache)
;;  (λ _ (printf "No Network Or Cache Available\n"))) ;; isn't currently used

(define (cache-valid? filename)
  (if (and (file-exists? filename) (< (get-file-age filename) max-cache-age-hours)) #t
      #f)
  )

(define (get-weather url stream-type city state-country)
  (let ([filename (string-append "weather_data/" stream-type "/" city " - " state-country ".json")])
    (cond
      [(equal? (cache-valid? filename) #t) (if (string=? stream-type "current")
                                               (parse-weather-current filename)
                                               (create-forecast-list (parse-weather-forecast filename) null))]  
      [(and (download-weather url stream-type city state-country)
            (cache-valid? filename)) (if (string=? stream-type "current")
                                         (parse-weather-current filename)
                                         (create-forecast-list (parse-weather-forecast filename) null))]
      [else '()])
    ))

(define (download-weather url stream-type city state-country)
  (with-handlers ([exn:fail:network? (network-failure)])
    (fetch-weather url stream-type city state-country))
    )

(define (get-file-age filename)
  (file-or-directory-modify-seconds filename #f)
  (let ([now (now)]
        [last (posix->datetime (file-or-directory-modify-seconds filename #f))])
        (hours-between last now))      
  )

(define (fetch-weather url stream-type city state-country)
  (let ([file-path (string-append "weather_data/" stream-type "/" city " - " state-country ".json")])
    (let ([remote-data
           (port->string (get-pure-port
                          (string->url
                           (string-append url (string-append city "," state-country) options "&appid=" weather-key))))])
        (write-out file-path remote-data)
  )))

(define (write-out file-path data)
  (call-with-output-file* file-path #:exists 'replace #:mode 'text
   (lambda (x)
     (display data x))))

;; Parsers
(define (parse-weather-current filename)
  (define json-data (call-with-input-file filename read-json))
  (define main-val (hash-ref json-data 'main))
  (define wind-val (hash-ref json-data 'wind))
  (define weather-val (car (hash-ref json-data 'weather)))
  
  (define current-weather (list
                   (cons "city"
                         (hash-ref json-data 'name))
                   (cons "current"
                         (hash-ref main-val 'temp))
                   (cons "high"
                         (hash-ref main-val 'temp_max))
                   (cons "low"
                         (hash-ref main-val 'temp_min))
                   (cons "humidity"
                         (hash-ref main-val 'humidity))
                   (cons "wind"
                         (/ (hash-ref wind-val 'speed) 0.3048)) ;convert from m/s to ft/s
                   (cons "description"
                         (hash-ref weather-val 'main))
                   ))
  current-weather
  )


(define (create-forecast-list data results-list)
    (if (null? data) (cons results-list null)
        (if (null? results-list) (create-forecast-list (cdr data) (forecast (car data)))
            (cons results-list (create-forecast-list (cdr data) (forecast (car data)))))))


(define (parse-weather-forecast filename)
  (define json-data (call-with-input-file filename read-json))
  (hash-ref json-data 'list))
  
(define convert-date (λ (format dt)
                        (format (->date (posix->datetime dt)))))

(define (make-dir path)
  (if (directory-exists? path) null
      (make-directory path))
  )

(define (forecast json-data)
  (define dt (hash-ref json-data 'dt))
  (define temp-val (hash-ref json-data 'temp))
  (define weather-val (car (hash-ref json-data 'weather)))
  (define forecast-weather  
    (cons (convert-date date->iso8601 dt)
          (list
           (cons "high" (hash-ref temp-val 'max))
           (cons "low" (hash-ref temp-val 'min))
           (cons "description" (hash-ref weather-val 'main))
           (cons "date" (list
                         (cons "month" (convert-date ->month dt))
                         (cons "day" (convert-date ->day dt))
                         (cons "year" (convert-date ->year dt))
                         (cons "day-of-week" (name-of-day (convert-date ->wday dt))))))))
  forecast-weather
  )

(define (name-of-day n)
  (cond [(= 0 n) "Sunday"]
        [(= 1 n) "Monday"]
        [(= 2 n) "Tuesday"]
        [(= 3 n) "Wednesday"]
        [(= 4 n) "Thursday"]
        [(= 5 n) "Friday"]
        [else "Saturday"]))

