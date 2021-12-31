#lang racket
(require "../chat/message/main.rkt"
         "../nlp/tagging.rkt"
         "tools/weather.rkt"
         "tools/pic.rkt"
         "tools/joke.rkt"
         "tools/dict.rkt"
         "tools/osu.rkt"
         "tools/dice.rkt"
         "tools/remind/main.rkt")

(provide handle-message)


(define (handle-message bot event add-message)
  (define message (send event get-message))
  (define sender (send event get-sender))
  (define subject (send event get-subject))
  (define sender-id (send (send event get-sender) get-id))
  (define message-content (send message content-to-string))
  
  (define tagged-words (cut-tagged-words message-content))
  (define raw-words (map tagged-word-data tagged-words))

  (define matched #f)
  (when (not matched)
    (set! matched #t)
    (match raw-words
      [(app query-weather-parse-args (? list? args))
       (apply query-weather `(,@args ,add-message))]
      [(list "动漫" (or "图" "图片"))
       (get-random-pic "anime" add-message)]
      [(list "风景" (or "图" "图片"))
       (get-random-pic "scenery" add-message)]
      [(list (or "美女" "妹子") (or "图" "图片"))
       (get-random-pic "mm" add-message)]
      [(list (or "图" "图片"))
       (get-random-pic "all" add-message)]

      [(list "取消" "提醒")
       (cancel-remind sender add-message)]
     
      [(list "笑话")
       (get-joke add-message)]
      [(list "随机" "点数")
       (random-dice add-message)]
    
      [(list "美音" args ...)
       (dict-a-voice (string-join args " ") add-message)]
      [(list "英音" args ...)
       (dict-b-voice (string-join args " ") add-message)]

      ; osu
      [(list "osu" "菜单")
       (osu-menu add-message)]
      [(list "设置" "osu" "用户" osu-uid)
       (sender-bind-to-osu-user sender-id osu-uid add-message)]
      [(list "osu-stat" osu-uid ...)
       (osu-stat "osu"
                 (if (null? osu-uid) #f (car osu-uid))
                 sender-id
                 add-message)]
    
      [_ (set! matched #f)]))
  (when (not matched)
    (set! matched #t)
    (match tagged-words
      [(app (remind-parse-args sender) (? list? args))
       (apply make-remind `(,subject ,@args ,add-message))]
      [_ (set! matched #f)]))
  matched)
  