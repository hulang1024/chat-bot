#lang racket
(require net/url
         json)

(provide query-weather
         query-weather-parse-args)


(define weather-icons
  (hash
   ;晴
   "0" "☀️"
   ;晴
   "1" "🌙"
   ;多云
   "4" "⛅️"
   ;阴
   "9" "☁️"
   ;小雨
   "13" "🌧"
   ;雨夹雪
   "20" "🌨"))

(define (get-icon k)
  (if (hash-has-key? weather-icons k) (hash-ref weather-icons k) ""))
  
(define api-key "YK6GIIA0EB")

(define (query-weather location spec-rel-date add-message)
  (define (create-url location days)
    (string->url (format "https://api.seniverse.com/v3/weather/daily.json?location=~A&language=~A&unit=c&start=0&days=~A&key=~A"
                         location "zh-Hans" days api-key)))

  (define-values (status headers in)
    (http-sendrecv/url (create-url location 3)))
  (define api-result (string->jsexpr (port->string in)))
  (cond
    [(hash-has-key? api-result 'results)
     (define (day-line day prefix)
       (add-message (format "~A~A ~A，晚间~A~A，~A ~~ ~A°C，~A风~A级(风速~Akm/h)\n"
                            prefix
                            (get-icon (hash-ref day 'code_day))
                            (hash-ref day 'text_day)
                            (get-icon (hash-ref day 'code_night))
                            (hash-ref day 'text_night)
                            (hash-ref day 'low)
                            (hash-ref day 'high)
                            (hash-ref day 'wind_direction)
                            (hash-ref day 'wind_scale)
                            (hash-ref day 'wind_speed))))
     
     (define rel-dates '("今天" "明天" "后天"))

     (add-message (format "~A的天气\n"
                          (if spec-rel-date
                              (format "~a~a" location (list-ref rel-dates spec-rel-date))
                              location)))
     (define result (list-ref (hash-ref api-result 'results) 0))
     (define daily (hash-ref result 'daily))
     (cond
       [spec-rel-date
        (day-line (list-ref daily spec-rel-date) "")]
       [else 
        (define i (if spec-rel-date date 0))
        (for ((day daily))
          (day-line day
                    (string-append
                     (if (< i 3)
                         (list-ref rel-dates i)
                         (hash-ref day 'date))
                     "："))
          (set! i (+ i 1)))])
     #t]
    [else
     (add-message (format "未查询到位置 ~A 的天气\n" location))
     #f]))


(define (query-weather-parse-args words)
  (match words
    [(list city (or "今天" "明天" "后天") (or "的天气" "天气"))
     (define day-name (list-ref words 1))
     (define day (match day-name ["今天" 0] ["明天" 1] ["后天" 2] [_ #f]))
     (list city day)]
    [(list city "天气")
     (list city #f)]
    [else #f]))