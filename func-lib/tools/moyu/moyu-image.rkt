#lang racket
(require racket/date
         racket/draw
         2htdp/batch-io
         "config.rkt")

(provide make-moyu-image
         rgb)


(define holiday-file-path (build-path work-path "holiday.csv"))
(define holidays #f)

(define (make-moyu-image draw-more)
  (define image (render-moyu-template-image
                 (build-path work-path "moyu-template.png")
                 (make-moyu-data)))
  (draw-more image)
  (define path (build-path work-path "moyu.png"))
  (send image save-file path 'png 100)
  path)


(define (make-moyu-data)
  (when (not holidays)
    (set! holidays (read-holidays)))
  (define now (current-date))
  (define (get-holiday-rest-day name)
    (define seconds (hash-ref holidays name #f))
    (cond
      [seconds
       (define diff-seconds (- seconds (date->seconds now)))
       (define diff-days (floor (/ diff-seconds 86400)))
       (if (>= diff-days 0) diff-days (+ 365 diff-days))]
      [else 0]))

  (define today-text
    (format "~a月~a日周~a~a"
            (leftpad (date-month now) 2 "0")
            (leftpad (date-day now) 2 "0")
            (string-ref "日一二三四五六" (date-week-day now))
            (get-hour-section-name (date-hour now))))

  (define offwork-time
    (find-seconds 0 0 18
                  (date-day now) (date-month now) (date-year now)))
  (define offwork-rest-minutes
    (max 0 (floor (/ (- offwork-time (date->seconds now)) 60))))
  `((after ,(format "~a分钟" 120))
    (today ,today-text)
    (下班 ,offwork-rest-minutes)
    (周末 ,(max 0 (- 5 (date-week-day now))))
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
  (define rows (read-csv-file (path->string holiday-file-path)))
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

(define (get-hour-section-name hour)
  (define section
    (findf
     (λ (it)
       (match-define (list start end) (cadr it))
       (if (< start end)
           (<= start hour end)
           (<= start hour)))
     `(["凌晨" (0 2)]
       ["黎明" (3 4)]
       ["拂晓" (5 6)]
       ["早上" (7 8)]
       ["上午" (9 11)]
       ["中午" (11 13)]
       ["下午" (14 17)]
       ["傍晚" (17 18)]
       ["晚上" (18 22)]
       ["午夜" (23 1)])))
  (car section))
    
(define (leftpad v w p)
  (~a v #:width w #:align 'right #:pad-string p))

(define (render-moyu-template-image template-path render-data)
  (define (get-value name)
    (cadr (assoc name render-data)))
  
  (define moyu-bitmap (read-bitmap template-path))
  (define dc (send moyu-bitmap make-dc))

  ; 多少时间之后
  (send dc set-font (make-font #:size 12.5
                               #:family	'modern
                               #:face "FZLanTingHeiS-R-GB"))
  (send dc set-text-foreground (rgb 10 10 10))
  (send dc draw-text (get-value 'after) 106 368)

  ; 今日
  (send dc set-font (make-font #:size 11
                               #:family	'modern
                               #:face "FZLanTingHeiS-R-GB"))
  (send dc set-text-foreground (rgb 18 128 20))
  (define base-y 431)
  (send dc draw-text (get-value 'today) 170 base-y)
  (send dc set-text-foreground (rgb 0 0 0))
  (define hi-text "好，摸鱼人。")
  (send dc draw-text hi-text 296 base-y)


  (define (draw-digital x y text color)
    (send dc set-font (make-font #:size 12
                                 #:family 'modern
                                 #:face "WW Digital"
                                 #:weight 'semibold))
    (send dc set-text-foreground color)
    ; 为等宽分别画每个字符
    (for ([c text])
      (when (not (char-whitespace? c))
        (send dc draw-text (string c) x y))
      (set! x (+ x 10))))

  (define (draw-time-unit x y text)
    (send dc set-text-foreground (rgb 0 0 0))
    (send dc set-font (make-font #:size 11
                                 #:face "FZLanTingHeiS-R-GB"
                                 #:family 'modern))
    (send dc draw-text text x (+ y 2.5)))

  ; 时间距离相关
  (define base-x 141)
  (set! base-y 528)
  (define line-height 24.5)
  ; 下班
  (define (draw-offwork)
    (define seconds (get-value '下班))
    (define hour (floor (/ seconds 60)))
    (define minute (remainder seconds 60))
    (define color (rgb 252 15 29))

    (define x base-x)
    (when (> hour 0)
      (draw-digital x base-y (leftpad hour 3 " ") color)
      (draw-time-unit (+ x 32) base-y "小时")
      (set! x (+ x 64)))
    (define minute-text (if (> hour 0)
                            (number->string minute)
                            (leftpad minute 3 " ")))
    (draw-digital x base-y minute-text color)
    (set! x (+ x (* (string-length minute-text) 10) 1))
    (draw-time-unit x base-y "分钟"))
  
  ; 节假日剩余
  (define line 1)
  (define (draw-holiday name color [postfix "天"])
    (draw-digital base-x
                  (+ base-y (* line-height line))
                  (leftpad (get-value name) 3 " ")
                  color)
    (draw-time-unit (+ base-x 32)
                    (+ base-y (* line-height line))
                    postfix)
    (set! line (+ line 1)))

  (draw-offwork)    
  (draw-holiday '周末 (rgb 253 141 37))
  (draw-holiday '春节 (rgb 18 128 20))
  (draw-holiday '清明节 (rgb 0 153 255))
  (draw-holiday '劳动节 (rgb 204 0 255))
  (draw-holiday '端午节 (rgb 51 0 255))
  (draw-holiday '中秋节 (rgb 255 66 255))
  (draw-holiday '国庆节 (rgb 255 161 38))
  (draw-holiday '元旦 (rgb 0 153 255))
  moyu-bitmap)

(define (rgb r g b)
  (make-object color% r g b))