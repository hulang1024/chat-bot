#lang racket
(require racket/draw
         "config.rkt"
         "../holiday.rkt")

(provide make-moyu-image
         rgb)

(define (make-moyu-image draw-more)
  (define image (render-moyu-template-image
                 (build-path work-path "moyu-template.png")
                 (make-holiday-data)))
  (draw-more image)
  (define path (build-path work-path "moyu.png"))
  (send image save-file path 'png 100)
  path)


(define (render-moyu-template-image template-path render-data)
  (define get-value (get-holiday-value render-data))
  
  (define moyu-bitmap (read-bitmap template-path))
  (define dc (send moyu-bitmap make-dc))

  ; 今日
  (send dc set-font (make-font #:size 12
                               #:family	'modern
                               #:face "FZLanTingHeiS-R-GB"))
  (send dc set-text-foreground (rgb 18 128 20))
  (define base-y 409)
  (send dc draw-text (get-today-text) 178 base-y)
  (send dc set-text-foreground (rgb 0 0 0))
  (define hi-text "好，摸鱼人。")
  (send dc draw-text hi-text 315 base-y)

  (define digit-weight 12)
  (define (draw-digital x y text color)
    (send dc set-font (make-font #:size 15
                                 #:family 'modern
                                 #:face "WW Digital"
                                 #:weight 'thin))
    (send dc set-text-foreground color)
    ; 为等宽分别画每个字符
    (for ([c text])
      (when (not (char-whitespace? c))
        (send dc draw-text (string c) x y))
      (set! x (+ x digit-weight))))

  (define (draw-time-unit x y text)
    (send dc set-text-foreground (rgb 0 0 0))
    (send dc set-font (make-font #:size 11
                                 #:face "FZLanTingHeiS-R-GB"
                                 #:family 'modern))
    (send dc draw-text text x (+ y 6)))

  ; 时间距离相关
  (define base-x 138)
  (set! base-y 521)
  (define line-height 24.5)
  ; 下班
  (define (draw-offwork)
    (define seconds (get-offwork-rest-seconds))
    (define hour (floor (/ seconds 60)))
    (define minute (remainder seconds 60))
    (define color (rgb 252 15 29))

    (define x base-x)
    (when (> hour 0)
      (draw-digital x base-y (leftpad hour 3 " ") color)
      (draw-time-unit (+ x (* 3 digit-weight) 2) base-y "小时")
      (set! x (+ x (* 6 digit-weight) 4)))
    (define minute-text (if (> hour 0)
                            (number->string minute)
                            (leftpad minute 3 " ")))
    (draw-digital x base-y minute-text color)
    (set! x (+ x (* (string-length minute-text) digit-weight) 1))
    (draw-time-unit x base-y "分钟"))
  
  ; 节假日剩余
  (define line 1)
  (define (draw-holiday name color [postfix "天"])
    (draw-digital base-x
                  (+ base-y (* line-height line))
                  (leftpad (get-value name) 3 " ")
                  color)
    (draw-time-unit (+ base-x (* 3 digit-weight) 2)
                    (+ base-y (* line-height line))
                    postfix)
    (set! line (+ line 1)))

  (draw-offwork)    
  (draw-holiday '本周末 (rgb 253 141 37))
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