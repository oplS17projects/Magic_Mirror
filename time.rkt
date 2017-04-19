#lang racket
(require net/url json 2htdp/batch-io gregor gregor/time)

(require (file "api-keys.rkt"))

(provide get-time)


;; Current weather URL needed to get lat and long for timezonedb
(define weather-options "&mode=json&units=imperial&cnt=7")
(define weather-url "http://api.openweathermap.org/data/2.5/weather?q=")
(define timezonedb-url
  (string-append "http://api.timezonedb.com/v2/get-time-zone?key=" time-key "&format=json&by=position&"))

(define (get-time city state-country)
  (let ([coords (coordinates city state-country)])
    (let ([timezonedb (string-append timezonedb-url
                                     "lat=" (number->string (cdar coords))
                                     "&lng=" (number->string (cdadr coords)))])
      (let ([remote-data (read-json (get-pure-port (string->url timezonedb)))])
        (let ([zone (hash-ref remote-data 'zoneName)])
          (cons zone (javascript-date (hash-ref remote-data 'timestamp))))))))

       
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

;;FORMAT: new Date(year, month, day, hours, minutes, seconds, milliseconds);
(define (javascript-date data)
  (let ([year (parse-date-time ->year ->date data)])
    (let ([month (parse-date-time ->month ->date data)])
      (let ([day (parse-date-time ->day ->date data)])
        
        (let ([hours (parse-date-time ->hours ->time data)])
          (let ([minutes (parse-date-time ->minutes ->time data)])
            (let ([seconds (parse-date-time ->seconds ->time data)])
              
              (string-append (number->string year) ","
                             (number->string month) ","
                             (number->string day) ","
                             (number->string hours) ","
                             (number->string minutes) ","
                             (number->string seconds)))))))))


(define parse-date-time (λ (format proc dt)
                          (format (proc (posix->datetime dt)))))

