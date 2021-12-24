#lang racket
(require "tools/weather.rkt"
         "tools/pic.rkt"
         "tools/joke.rkt"
         "tools/dict.rkt"
         "tools/osu.rkt"
         "tools/dice.rkt")

(provide handle-cmdline-message)


(define (handle-cmdline-message m sender add-message)
  (define m-str (send m content-to-string))
  (define cmd-name/args (string-split m-str " "))

  (define sender-id (send sender get-id))

  (match cmd-name/args
    [(list (app query-weather-parse-args-from-text (? list? ret)))
     (apply query-weather `(,@ret ,add-message))]
    [(list "图片")
     (get-random-pic add-message)]
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
  