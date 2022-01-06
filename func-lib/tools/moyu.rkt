#lang racket
(require racket/date
         racket/draw
         net/url
         json
         "../../chat/message/main.rkt")

(provide make-moyu)

(define temp-path "/home/chat-bot/func-lib/tools/")
(define moyu-template-path (string-append temp-path "moyu-template.png"))
(define last-make-date #f)

(define (make-moyu add-image)

  (define now (current-date))
  (define today-string (format "~a-~a-~a"
                               (date-year now)
                               (date-month now)
                               (date-day now)))
  (define path (string-append temp-path "moyu-today.png"))
  (when (or (not last-make-date)
            (not (string=? last-make-date today-string)))
    (generate-moyu-image (calc-moyu-data now) path)
    (set! last-make-date today-string))
  (add-image (new image-message% [path path])))


(define (holiday-api)
  (define url (string->url "http://timor.tech/api/holiday/year"))
  (define-values (status headers in) (http-sendrecv/url url))
  (define result (string->jsexpr (port->string in)))
  (if (= (hash-ref result 'code) 0)
      (sort (hash-values (hash-ref result 'holiday))
            <
            #:key (λ (item)
                    (define date-str (hash-ref item 'date))
                    (define y (substring date-str 0 4))
                    (define m (substring date-str 5 7))
                    (define d (substring date-str 8 10))
                    (string->number (string-append y m d))))
                    
      #f))

(define (calc-moyu-data now)
  (define (leftpad-0 v [w 2])
    (~a v #:width 2 #:align 'right #:pad-string "0"))

  (define (date-str->seconds date-str)
    (define y (string->number (substring date-str 0 4)))
    (define m (string->number (substring date-str 5 7)))
    (define d (string->number (substring date-str 8 10)))
    (find-seconds 0 0 0 d m y))

  (define holidays (holiday-api))

  (define (calc-rest-day name)
    (define found (findf (λ (item)
                           (string=? (hash-ref item 'name) name))
                         holidays))
    (cond
      [found
       (define diff-seconds (- (date-str->seconds (hash-ref found 'date))
                         (date->seconds now)))
       (define diff-days (floor (/ diff-seconds 86400)))
       (if (> diff-days 0) diff-days (- 365 diff-days))]
      [else 0]))

  (define today-text
    (format "~a月~a日"
            (leftpad-0 (date-month now))
            (leftpad-0 (date-day now))))
  `((after ,(format "~a分钟后" 5))
    (today ,today-text)
    (rest-days-weekend ,(- 5 (date-week-day now)))
    (rest-days-spring-festival ,(calc-rest-day "除夕"))
    (rest-days-qingming ,(calc-rest-day "清明节"))
    (rest-days-worker ,(calc-rest-day "劳动节"))
    (rest-days-dragon-boat ,(calc-rest-day "端午节"))
    (rest-days-mid-autumn ,(calc-rest-day "中秋节"))
    (rest-days-national-day ,(calc-rest-day "国庆节"))
    (rest-days-new-year-day ,(calc-rest-day "元旦"))))


(define (generate-moyu-image moyu-data save-path)
  (define (get-value name)
    (cadr (assoc name moyu-data)))
  
  (define moyu-bitmap (read-bitmap moyu-template-path))
  (define dc (new bitmap-dc% [bitmap moyu-bitmap]))
  (define (rgb r g b)
    (make-object color% r g b))

  ; 多少时间之后
  (send dc set-font (make-font #:size 12
                               #:weight 'bold))
  (send dc set-text-foreground (rgb 10 10 10))
  (send dc draw-text (get-value 'after) 86 304)

  (send dc set-font (make-font #:size 10
                               #:weight 'bold))
  ; 今日
  (send dc set-text-foreground (rgb 18 128 20))
  (send dc draw-text (get-value 'today) 132 374)
  ; 距离天数
  (define (draw-day day x y color)
    (send dc set-text-foreground color)
    (send dc draw-text (number->string day) x y))
  (define line-h 18)
  (define base-y 452)
  (draw-day (get-value 'rest-days-weekend) 101 base-y (rgb 252 15 29))
  (draw-day (get-value 'rest-days-spring-festival) 102 (+ base-y line-h) (rgb 253 212 49))
  (draw-day (get-value 'rest-days-qingming) 115 (+ base-y (* line-h 2)) (rgb 18 128 20))
  (draw-day (get-value 'rest-days-worker) 115 (+ base-y (* line-h 3)) (rgb 78 255 254))
  (draw-day (get-value 'rest-days-dragon-boat) 115 (+ base-y (* line-h 4)) (rgb 41 54 251))
  (draw-day (get-value 'rest-days-mid-autumn) 115 (+ base-y (* line-h 5)) (rgb 180 87 241))
  (draw-day (get-value 'rest-days-national-day) 115 (+ base-y (* line-h 6)) (rgb 139 18 138))
  (draw-day (get-value 'rest-days-new-year-day) 101 (+ base-y (* line-h 7)) (rgb 253 141 37))
  (send moyu-bitmap save-file save-path 'png))
