#lang racket
(require "tools/weather.rkt"
         "tools/pic.rkt"
         "tools/joke.rkt"
         "tools/dict.rkt"
         "tools/osu.rkt"
         "tools/dice.rkt"
         "tools/remind.rkt")

(provide handle-message)


(define (handle-message bot event add-message)
  (define message (send event get-message))
  (define sender (send event get-sender))
  (define subject (send event get-subject))
  (define m-str (send message content-to-string))
  (define words (string-split m-str " "))

  (define sender-id (send (send event get-sender) get-id))

  (match words
    [(list (app query-weather-parse-args-from-text (? list? ret)))
     (apply query-weather `(,@ret ,add-message))]
    [(list (regexp #rx"动漫图片?$"))
     (get-random-pic "anime" add-message)]
    [(list (regexp #rx"风景图片?$"))
     (get-random-pic "scenery" add-message)]
    [(list (regexp #rx"妹子图片?$"))
     (get-random-pic "mm" add-message)]
    [(list (regexp #rx"图片?$"))
     (get-random-pic "all" add-message)]

    [(list (app remind/time-parse-args-from-text (? list? ret)))
     (apply make-remind/time `(,subject ,sender ,@ret ,add-message))]
    [(list "取消提醒")
     (cancel-remind sender add-message)]
     
    [(list "笑话")
     (get-joke add-message)]
    [(list "随机点数")
     (random-dice add-message)]
    
    [(list "美音" args ...)
     (dict-a-voice (string-join args " ") add-message)]
    [(list "英音" args ...)
     (dict-b-voice (string-join args " ") add-message)]

    ; osu
    [(list "osu菜单")
     (osu-menu add-message)]
    [(list "设置osu用户" osu-uid)
     (sender-bind-to-osu-user sender-id osu-uid add-message)]
    [(list "osu-stat" osu-uid ...)
     (osu-stat "osu"
               (if (null? osu-uid) #f (car osu-uid))
               sender-id
               add-message)]
    
    [_ #f]))
  