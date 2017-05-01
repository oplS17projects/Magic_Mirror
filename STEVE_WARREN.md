# Magic Mirror

## Steve Warren
### April 30, 2017

# Overview
My portion of the project was the weather, time, word-of-the-day (wotd) modules and the preferences GUI/module.  The modules served to provide the front end with formatted data from various sources around the Internet.  The GUI provided a simplified way of setting preferences without requiring additional burden to the end user.  The module portion of the preferences allowed the front end to request the time and weather preferences that the user has chosen.

# Libraries Used
Preferences used:
```Racket
#lang racket/gui
(require json)
```

* The racket/gui language was used as it combined all the binding of the racket language, gui and draw modules.  This not only appears cleaner code-wise (IMHO) but also provided me the necessary bindings to create the GUI portion of the interface.
* json was used to storing the preference file locally and to read the file as required by the front-end.

Weather used:
```Racket
(require net/url)  
(require json)
(require gregor)
```
* net/url was used to read data from [OpenWeatherMap](http://openweathermap.org/).
* The data requested from OpenWeatherMap was in json format, so the library was included to read the data to allow for parsing.
* gregor was used to convert from Posix to Gregorian date and time.

Time used:
```Racket
(require net/url)
(require json)
(require gregor)
```
* net/url was used to get the weather from OpenWeatherData, which provides both lattitude and longitude data for a given location.  This information was then parsed and used to request the date and time from [timezonedb](https://timezonedb.com/).  This allowed me to save the cost subscription as retrieving timezone information is not free if you want to query by using city/state/country names.
* json was used to parse the data from both websites
* gregor was used to convert from Posix to Gregorian date and time. 

WOTD used:
```Racket
(require net/url)
```
* net/url was used to parse the data scraped from [Dictionary.com](http://www.dictionary.com/wordoftheday/). The coding goal of this module was to try my hand at webscraping as the other modules all used APIs.

# Key Code Excerpts

Below are some of the key concepts that embody the concepts of OPL that were implemented into the project... 

## 1. Function Composition

While not the most technical piece of code, this was my "I get it" moment.  Initially I was only parsing the time and ignoring the date.  As such, I created a function for the time and actually created two separate functions; one function used a lambda, and the other didn't. While I posited the question in the code "what's the difference?" I almost immediately forgot the question.  Some days later I realized that I would also need the date.  As I wrote a separate date function above the time.  It wasn't until a made a later pass over my code that I realized that they were closely related that they didn't need to be separate function but could be combined into a following:

```Racket
(define parse-date-time (λ (format proc dt)
                          (format (proc (posix->datetime dt)))))
```

This can be used to request date or time from a given Posix timestamp as follows:

```Racket
(let ([year (parse-date-time ->year ->date data)])
...
(let ([seconds (parse-date-time ->seconds ->time data)])
```

## 2. Procedural Abstraction
Each of the modules makes use of procedural abstraction by only exposing specific functions to the other files via provide, and requiring a minimal amount of information needed to be used successfully.

Getting and setting preferences, and word-of-the-day require no additional parameters to work.  Weather and time use a very similar format in order to obtain their results.  Time also obscures the use of OpenWeatherMap in retrieving its data.

## 3. Using Recursion to Accumulate Results
The word-of-the-day module scrapes its data from Dictionary.com, and recursively parses the data for a maximum of 5 definitions.  This limit was put into place in the event that a particular word had too many definitions I wanted to prevent the word from taking up too much space on the mirror.  There's actually quite a bit of OPL ideas within these few lines.

```Racket
(define (def num items)
  (let ([url "http://www.dictionary.com/wordoftheday/"]
        [rx (string-append "(?<=<li class=\"" (cdr (assoc num kv)) "\"><span>).*?(?=</span>)")])
    (define result (regexp-match rx (get-pure-port (string->url url))))
    
   (if (or (false? result) (> num 5) (< num 1)) (reverse items)
        (def (+ num 1) (cons (string-append (number->string num) ": " (bytes->string/utf-8 (car result))) items)))))
```

## 4. BYOC: Bring Your Own Code
Early on I was using the 2htdp/batch-io library for writing files. Still in the "I don't get it" stage, I was unable to work out how to write files without the nearly-impossible-to-pronounce library. Eventually I was able to replace this library with my own code. While it doesn't exactly bring me to the bare-metal, it gets me one layer closer.

```Racket
(define (write-out file-path data)
  (call-with-output-file* file-path #:exists 'replace #:mode 'text
   (lambda (x)
     (display data x))))
```

## 5. Discovery
If I were to sum up this project with one word, it would be discovery.  The project allowed me the ability to work with and think about Racket and recursive programming in a way that I wasn't able to with the previous assignments.  While the majority of the code would serve as an example, I chose the following since I was particularly fond of it. I was able to make use of a non-list data structure and do so in a way that was both clean in its implementation and clear enough to be understood without needing to resort to a sea of comments.

```Racket
(struct prefs (w-city w-state d-city d-state))
...
(define (prefs->jsexpr p)
  (hasheq 'weather-city   (prefs-w-city p)
          'weather-state  (prefs-w-state p)
          'datetime-city  (prefs-d-city p)
          'datetime-state (prefs-d-state p)))
```