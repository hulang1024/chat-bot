#lang racket
(require net/url
         json)

(provide query-weather)


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

(define (query-weather location add-message)
  (define (create-url location days)
    (string->url (format "https://api.seniverse.com/v3/weather/daily.json?location=~A&language=~A&unit=c&start=0&days=~A&key=~A"
                         location "zh-Hans" days api-key)))

  (define-values (status headers in)
    (http-sendrecv/url (create-url location 3)))
  (define api-result (string->jsexpr (port->string in)))
  (if (hash-has-key? api-result 'results)
      (let ([result (list-ref (hash-ref api-result 'results) 0)]
            [rel-dates '("今天" "明天" "后天")]
            [i 0])
        (add-message (format "~A的天气\n" location))
        (for ((day (hash-ref result 'daily)))
          (add-message (format "~A：~A ~A，晚间~A~A，~A ~~ ~A°C，~A风~A级(风速~Akm/h)\n"
                               (if (< i 3)
                                   (list-ref rel-dates i)
                                   (hash-ref day 'date))
                               (get-icon (hash-ref day 'code_day))
                               (hash-ref day 'text_day)
                               (get-icon (hash-ref day 'code_night))
                               (hash-ref day 'text_night)
                               (hash-ref day 'low)
                               (hash-ref day 'high)
                               (hash-ref day 'wind_direction)
                               (hash-ref day 'wind_scale)
                               (hash-ref day 'wind_speed)))
          (set! i (+ i 1)))
        #t)
      (begin
        (add-message (format "未查询到位置 ~A 的天气\n" location))
        #f)))
