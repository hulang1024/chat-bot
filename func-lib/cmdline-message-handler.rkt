#lang racket
(require "tools/weather.rkt"
         "tools/pic.rkt"
         "tools/joke.rkt"
         "tools/dict.rkt"
         "tools/osu.rkt")

(provide handle-cmdline-message)


(define (handle-cmdline-message m sender add-message)
  (define m-str (send m content-to-string))
  (define cmd-name/args (string-split m-str " "))

  (define sender-id (send sender get-id))

  (match cmd-name/args
    [(list "天气" location)
     (query-weather location add-message)]
    [(list "图片")
     (get-random-pic add-message)]
    [(list "笑话")
     (get-joke add-message)]
    
    [(list "美音" args ...)
     (dict-a-voice (string-join args " ") add-message)]
    [(list "英音" args ...)
     (dict-b-voice (string-join args " ") add-message)]

    ; osu
    [(list "设置osu用户" osu-uid)
     (sender-bind-to-osu-user sender-id osu-uid add-message)]
    
    
    [_ #f]))
  