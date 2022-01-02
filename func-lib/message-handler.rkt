#lang racket
(require racket/date
         "../chat/message/main.rkt"
         "../chat/contact/group.rkt"
         "../nlp/tagging.rkt"
         "../nlp/time.rkt"
         "say-to-me.rkt"
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
  (define subject (send event get-subject))
  (when (or (not (is-a? subject group%))
            (send bot call-me? message))
    (define message-content (send message content-to-string))
    (define tagged-words (cut-tagged-words message-content))
    (define (remove-lead-wps)
      (set! tagged-words
            (let loop ([words tagged-words])
              (if (and (not (null? words))
                       (equal? 'wp (tagged-word-type (car words))))
                  (loop (cdr words))
                  words))))
    (match tagged-words
      [(list (tagged-word 'wp "@") (tagged-word 'number id) _ ...)
       (when (= id (send bot get-id))
         (set! tagged-words (list-tail tagged-words 2)))]
      [else (remove-lead-wps)])
    (when (and (not (null? tagged-words))
               (string=? (tagged-word/text-text (first tagged-words))
                         (send bot get-nickname)))
      (set! tagged-words (list-tail tagged-words 1))
      (remove-lead-wps))
    (cond
      [(null? tagged-words)
       (cond
         [(> (random) 0.5)
          (add-message (face-from-id 32))
          (add-message "有啥事?")]
         [else
          (add-message say-to-me)])]
      [else
       (define handled (main-handle-message bot event tagged-words add-message))
       (cond
         [handled #t]
         [else
          (cond
            [(> (random) 0.5)
             (add-message (make-quote-reply message))
             (add-message (face-from-id 176))
             (add-message "我还不懂")]
            [else
             (add-message say-to-me)])])])))

(define (main-handle-message bot event tagged-words add-message)
  (define sender (send event get-sender))
  (define sender-id (send (send event get-sender) get-id))
  
  (define text-words (map tagged-word/text-text tagged-words))

  (define matched #f)
  (when (not matched)
    (set! matched #t)
    (match text-words
      [(list (or "帮助" "菜单"))
       (add-message say-to-me)]
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
    
      [else (set! matched #f)]))
  (when (not matched)
    (set! matched #t)
    (match tagged-words
      [(list (tagged-word 'text "计算")
             (tagged-word 'text "时间")
             (tagged-word 'time time-word))
       (define now (current-date))
       (define ret-date (time-word->date time-word))
       (add-message (make-quote-reply (send event get-message)))
       (add-message (format "时间是 ~a" (date->short-string ret-date now)))]
      [(app (remind-parse-args event) (? list? args))
       (apply make-remind `(,@args ,add-message))]
      [else (set! matched #f)]))
  matched)
  