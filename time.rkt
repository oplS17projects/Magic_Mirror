#lang racket
(require net/url json 2htdp/batch-io gregor gregor/time)

(require (file "api-keys.rkt"))

(provide get-time)

;; TO-DO check on failure to connect to network and failure to DL cache

;; Current weather URL needed to get lat and long for timezonedb
(define weather-options "&mode=json&units=imperial&cnt=7")
(define weather-url "http://api.openweathermap.org/data/2.5/weather?q=")
(define timezonedb-url
  (string-append "http://api.timezonedb.com/v2/get-time-zone?key=" time-key "&format=json&by=position&"))

(define (get-time city state-country)
  (define coords (coordinates city state-country))
  (define latitude (cdar coords))
  (define longitude (cdadr coords))
  (define timezonedb (string-append timezonedb-url "lat=" (number->string latitude) "&lng=" (number->string longitude)))
  (define remote-data (read-json (get-pure-port (string->url timezonedb))))
  (define zone (hash-ref remote-data 'zoneName))
  (define time (convert-time (hash-ref remote-data 'timestamp)))
  (define date (convert-date date->iso8601 (hash-ref remote-data 'timestamp)))
  (cons zone (cons date time))
  )
         
(define (network-failure)
  (λ _ (printf "Network Unavailable\n")))

(define (cache-valid? filename)
  (if (and (file-exists? filename)) #t
      #f)
  )

(define (coordinates city state-country)
  (make-dir "weather_data")
  (make-dir "weather_data/current")

  (get-weather city state-country)
  
  )

(define (parse-coords filename)
  (define json-data (call-with-input-file filename read-json))
  (define coord-val (hash-ref json-data 'coord))
  
  (define coords (list
                   (cons "lat"
                         (hash-ref coord-val 'lat))
                   (cons "lng"
                         (hash-ref coord-val 'lon))
                   ))
  coords
  )

(define (get-weather city state-country)
  (define filename (string-append "weather_data/current/" city " - " state-country ".json"))
  (cond
    [(equal? (cache-valid? filename) #t) (parse-coords filename)]
    [(and (download-weather city state-country) (cache-valid? filename))
     (parse-coords filename)]
    [else '()])
  )

(define (download-weather city state-country)
  (with-handlers ([exn:fail:network? (network-failure)])
    (fetch-weather city state-country))
    )

(define (get-file-age filename)
  (file-or-directory-modify-seconds filename #f)
  (let ([now (now)]
        [last (posix->datetime (file-or-directory-modify-seconds filename #f))])
        (hours-between last now))      
  )

(define (make-dir path)
  (if (directory-exists? path) null
      (make-directory path))
  )

(define (fetch-weather city state-country)
  (let ([file-path (string-append "weather_data/current/"  city " - " state-country ".json")])
  (let ([remote-data
         (port->string (get-pure-port
                        (string->url
                         (string-append weather-url (string-append city "," state-country) weather-options "&appid=" weather-key))))])
    (write-file file-path remote-data)
  )))

(define convert-date (λ (format dt)
                       (format (->date (posix->datetime dt)))))

(define (convert-time n)
  (time->iso8601 (->time (posix->datetime n))))

; difference?
    (define convert-time-lambda
  (λ (n) (time->iso8601 (->time (posix->datetime n)))))