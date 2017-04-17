#lang racket/gui
(require json)

(provide get-preferences)

;; TODO: Determine whether to keep or axe the enable buttons
;; TODO: Need preferences for WOTD
(struct prefs (w-city w-state d-city d-state))

(define prefs-file "preferences.json")
(define default-prefs (prefs "Lowell" "MA" "Boston" "MA"))


(define (get-preferences)
  (unless (file-exists? prefs-file)
    (write-out prefs-file (jsexpr->string (prefs->jsexpr default-prefs))))
  
  (let ([data (call-with-input-file prefs-file read-json)])
    (list
     (cons "weather-city" (hash-ref data 'weather-city))
     (cons "weather-state" (hash-ref data 'weather-state))
     (cons "datetime-city" (hash-ref data 'datetime-city))
     (cons "datetime-state" (hash-ref data 'datetime-state)))))

(define frame (new frame%
                   [label "Preferences"]
                   [width 300]
                   [height 300]
                   [style '(no-resize-border)]))

(define gbpanel-weather
  (new group-box-panel%
       [parent frame]
       [min-width   300]
       [min-height   30]
       [label "Weather"]))

(define gbpanel-datetime
  (new group-box-panel%
       [parent frame]
       [min-width   300]
       [min-height   30]
       [label "Date and Time"]))


(define row-buttons
  (new horizontal-panel%
       [parent frame]
       [min-width   300]
       [min-height   50]
       [stretchable-height #f]))

(define panel-buttons
  (new horizontal-panel%
       [parent row-buttons]
       [min-width 300]
       [alignment '(right center)]
       [stretchable-height #f]))


(define txt-weather-city (new text-field%
     [parent gbpanel-weather]
     [label "City: "]
     [init-value
      (let ([data (call-with-input-file prefs-file read-json)])
        (hash-ref data 'weather-city))]))


(define txt-weather-state (new text-field%
     [parent gbpanel-weather]
     [label "State/Country: "]
     [init-value
      (let ([data (call-with-input-file prefs-file read-json)])
        (hash-ref data 'weather-state))]))


(define weather-cb-panel
  (new panel%
       [parent gbpanel-weather]
       [alignment '(left center)]
       [vert-margin 0]
       [horiz-margin 0]))


(define datetime-cb-panel
  (new panel%
       [parent gbpanel-datetime]
       [alignment '(left center)]
       [vert-margin 0]
       [horiz-margin 0]))


(define check-weather
  (new check-box%
       [parent weather-cb-panel]
       [label "Enabled"]
       [min-width   300]
       [min-height   0]
       [value #t]))


(define txt-datetime-city (new text-field%
     [parent gbpanel-datetime]
     [label "City: "]
     [init-value
      (let ([data (call-with-input-file prefs-file read-json)])
        (hash-ref data 'datetime-city))]))


(define txt-datetime-state
  (new text-field%
       [parent gbpanel-datetime]
       [label "State/Country: "]
       [init-value
      (let ([data (call-with-input-file prefs-file read-json)])
        (hash-ref data 'datetime-state))]))


(define check-datetime
  (new check-box%
       [parent gbpanel-datetime]
       [label "Enabled"]
       [min-width   300]
       [min-height   0]
       [value #t]))

(define (test x)
  (display x)
  )

(define btn-ok (new button%
                    [parent panel-buttons]
                    [label "Ok"]
                    ;;[callback (λ (text-field event)
                    ;;            (test (send txt-datetime-city get-value)))]))
                    [callback (λ (text-field event) (set-preferences))]))

(define prefsx
  (list #hasheq((weather-city . (send txt-datetime-city get-value)))
        #hasheq((test . "ing"))       
  ))


(define btn-cancel
  (new button%
       [parent panel-buttons]
       [label "Cancel"]
       [callback (λ (button event)
                   (send frame show #f))]))


;(struct prefs (w-city w-state d-city d-state))
(define (prefs->jsexpr p)
  (hasheq 'weather-city   (prefs-w-city p)
          'weather-state  (prefs-w-state p)
          'datetime-city  (prefs-d-city p)
          'datetime-state (prefs-d-state p)))


(define (write-out file-path data)
  (call-with-output-file* file-path #:exists 'replace #:mode 'text
   (lambda (x)
     (display data x))))

(define (set-preferences)
  (let ([data (prefs
               (send txt-weather-city get-value)
               (send txt-weather-state get-value)
               (send txt-datetime-city get-value)
               (send txt-datetime-state get-value))])
    (begin
      (write-out prefs-file (jsexpr->string (prefs->jsexpr data)))
      (send frame show #f))))




(send frame show #t)














