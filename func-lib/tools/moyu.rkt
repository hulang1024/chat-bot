#lang racket
(require racket/date
         racket/draw
         net/url
         json
         2htdp/batch-io
         "../../chat/message/main.rkt")

(provide make-moyu)


(define temp-path "/home/chat-bot/data/")
(define moyu-template-path (string-append temp-path "moyu-template.png"))
(define holiday-file-path (string-append temp-path "holiday.csv"))
(define last-make-date #f)
(define holidays #f)

(define (make-moyu add-message)
  (define now (current-date))
  (define today-string (format "~a-~a-~a"
                               (date-year now)
                               (date-month now)
                               (date-day now)))
  (define path (string-append temp-path "moyu-today.png"))
  (when (or (not last-make-date)
            (not (string=? last-make-date today-string)))
    (generate-moyu-image (make-moyu-data now) path)
    (set! last-make-date today-string))
  (add-message (new image-message% [path path])))


(define (make-moyu-data now)
  (when (not holidays)
    (set! holidays (read-holidays)))

  (define (get-holiday-rest-day name)
    (define seconds (hash-ref holidays name #f))
    (cond
      [seconds
       (define diff-seconds (- seconds (date->seconds now)))
       (define diff-days (floor (/ diff-seconds 86400)))
       (if (>= diff-days 0) diff-days (+ 365 diff-days))]
      [else 0]))

  (define today-text
    (format "~a月~a日"
            (leftpad (date-month now) 2 "0")
            (leftpad (date-day now) 2 "0")))

  (define offwork-time
    (find-seconds 0 0 18
                  (date-day now) (date-month now) (date-year now)))
  (define offwork-rest-minutes
    (max 0 (floor (/ (- offwork-time (date->seconds now)) 60))))
  `((after ,(format "~a分钟" 120))
    (hour-section-name ,(get-hour-section-name now))
    (today ,today-text)
    (下班 ,offwork-rest-minutes)
    (周末 ,(- 5 (date-week-day now)))
    ,@(map (λ (h) (list h (get-holiday-rest-day h)))
           '(春节 清明节 劳动节 端午节 中秋节 国庆节 元旦))))


(define (read-holidays)
  (define now (current-date))
  (define (holiday-date m d)
    (find-seconds 0 0 0 d m (date-year now)))
  (define holidays (make-hash `((元旦 . ,(holiday-date 1 1))
                                (春节 . ,(holiday-date 1 31))
                                (劳动节 . ,(holiday-date 5 1))
                                (国庆节 . ,(holiday-date 10 1)))))
  (define rows (read-csv-file holiday-file-path))
  (for-each
   (λ (i)
     (hash-set! holidays
                (string->symbol (cadr i))
                (date-string->seconds (car i))))
   rows)
  holidays)

(define (date-string->seconds date-str)
  (define y (string->number (substring date-str 0 4)))
  (define m (string->number (substring date-str 5 7)))
  (define d (string->number (substring date-str 8 10)))
  (find-seconds 0 0 0 d m y))

(define (get-hour-section-name now)
  (define hour (date-hour now))
  (define section
    (findf
     (λ (it)
       (match-define (list start end) (cadr it))
       (<= start hour end))
     `(["凌晨" (0 2)]
       ["黎明" (4 5)]
       ["拂晓" (4 6)]
       ["清晨" (6 7)]
       ["早晨" (6 8)]
       ["早上" (6 8)]
       ["上午" (8 11)]
       ["中午" (11 13)]
       ["下午" (14 17)]
       ["晚上" (18 22)]
       ["傍晚" (17 18)]
       ["黄昏" (16 17)]
       ["午夜" (23 1)]
       ["夜间" (19 5)])))
  (car section))
    
(define (leftpad v w p)
  (~a v #:width w #:align 'right #:pad-string p))

(define (generate-moyu-image render-data save-path)
  (define (get-value name)
    (cadr (assoc name render-data)))
  
  (define moyu-bitmap (read-bitmap moyu-template-path))
  (define dc (new bitmap-dc% [bitmap moyu-bitmap]))
  (define (rgb r g b)
    (make-object color% r g b))

  ; 多少时间之后
  (send dc set-font (make-font #:size 13
                               #:family	'modern
                               #:face "微软雅黑"
                               #:weight 'medium))
  (send dc set-text-foreground (rgb 10 10 10))
  (send dc draw-text (get-value 'after) 100 364)

  ; 今日
  (send dc set-font (make-font #:size 11
                               #:family	'modern
                               #:face "微软雅黑"
                               #:weight 'medium))
  (send dc set-text-foreground (rgb 18 128 20))
  (send dc draw-text (get-value 'today) 170 428)
  (send dc set-text-foreground (rgb 0 0 0))
  (send dc draw-text (string-append (get-value 'hour-section-name) "好，摸鱼人") 238 428)

  ; 节假日剩余
  (define line 0)
  (define (draw-holiday name color [unit "天"])
    (define base-x 150)
    (define base-y 528)
    (define line-height 24.5)
    (define value (get-value name))
    (define value-text (leftpad (number->string value) 3 " "))
    (send dc set-font (make-font #:size 12
                                 #:family 'modern
                                 #:face "WW Digital"
                                 #:weight 'semibold))
    (send dc set-text-foreground color)
    (send dc draw-text value-text
          base-x
          (+ base-y (* line-height line)))
    (send dc set-text-foreground (rgb 0 0 0))
    (send dc set-font (make-font #:size 11
                                 #:face "微软雅黑"
                                 #:family 'modern))
    (send dc draw-text unit
          (+ base-x (* (string-length value-text) 10) 1)
          (+ base-y (* line-height line)))
    (set! line (+ line 1)))
  
  (draw-holiday '下班 (rgb 252 15 29) "分钟")
  (draw-holiday '周末 (rgb 253 141 37))
  (draw-holiday '春节 (rgb 18 128 20))
  (draw-holiday '清明节 (rgb 0 153 255))
  (draw-holiday '劳动节 (rgb 204 0 255))
  (draw-holiday '端午节 (rgb 51 0 255))
  (draw-holiday '中秋节 (rgb 255 66 255))
  (draw-holiday '国庆节 (rgb 255 161 38))
  (draw-holiday '元旦 (rgb 0 153 255))
  (send moyu-bitmap save-file save-path 'png))
