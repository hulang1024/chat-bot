#lang racket
(require racket/date
         2htdp/batch-io
         "config.rkt"
         "../../../timer.rkt"
         "../../../chat/message/main.rkt"
         "../../../chat/contact/group.rkt"
         "../../../chat/contact/friend.rkt")

(provide remind-load
         make-remind
         cancel-remind
         remind-parse-args)


(define user-remind-timers (make-hash))

(define (format-remind-time time)
  (define (to-d2 v)
    (~a v #:width 2 #:align 'right #:pad-string "0"))
  (define date (seconds->date time))
  (define sec (date-second date))
  (string-append (format "~a:~a"
                         (to-d2 (date-hour date))
                         (to-d2 (date-minute date)))
                 (if (> sec 0)
                     (string-append ":" (to-d2 (number->string sec)))
                     "")))

(define (make-remind subject user time content add-message)
  (define now (current-date))
  (define user-id (send user get-id))
  (cond
    [(> time (date->seconds now))
     (cancel-timer user-id)
     (set-remind subject user-id time content #:save #t)
     (add-message (face-from-id 178))
     (add-message (format "好的，将在~a提醒你~a" (format-remind-time time) content))]
    [else
     (add-message "时间已过")]))


(define (cancel-remind user add-message)
  (define user-id (send user get-id))
  (cond
    [(cancel-timer user-id)
     (db-delete-remind user-id)
     (add-message "已经取消提醒啦")]
    [else
     (add-message (face-from-id 32))
     (add-message "你什么时候设置的提醒")]))


(define (remind-load bot)
  (define rows (read-csv-file db-filename))
  (for-each
   (λ (row)
     (match-define (list user-id group-id time content ...) row)
     (set! user-id (string->number user-id))
     (set! group-id (string->number group-id))
     (set! time (string->number time))
     (set! content (string-join content "，"))
     (define subject (if (> group-id 0)
                         (new group% [bot bot] [id group-id])
                         (new friend% [bot bot] [id user-id])))
     (set-remind subject user-id time content #:save #f))
   rows))


(define (cancel-timer user-id)
  (cond
    [(hash-has-key? user-remind-timers user-id)
     (define timer (hash-ref user-remind-timers user-id))
     (send timer stop)
     (hash-remove! user-remind-timers user-id)
     #t]
    [else #f]))


(define (set-remind subject user-id time content #:save save)
  (define (db-save-remind)
    (define out (open-output-file db-filename
                                  #:mode 'text
                                  #:exists 'append))
    (define group-id (if (is-a? subject group%)
                         (send subject get-id)
                         0))
    (define row (string-join
                  (list (number->string user-id)
                        (number->string group-id)
                        (number->string time)
                        content)
                  ", "))
    (write-string (string-append row "\n") out)
    (close-output-port out))
  
  (define (handle-timeout)
    (cancel-timer user-id)
    (db-delete-remind user-id)
    
    (define (repeat-thing t)
      (define mcb (new message-chain-builder%))
      (define add-message (create-add-message mcb))
      (when (is-a? subject group%)
        (add-message (new at% [target user-id])))
      (add-message (string-append " " content))
      (send subject send-message (send mcb build)))
    (define repeat-timer (new repeat-timer%
                       [on-repeat repeat-thing]
                       [times 3]
                       [seconds 1]))
    (send repeat-timer start))

  (when save
    (db-save-remind))

  (define timer (new timer%
                     [time time]
                     [on-timeout handle-timeout]))
  (hash-set! user-remind-timers user-id timer)
  (send timer start))


(define (db-delete-remind user-id)
  (define old-rows (read-csv-file db-filename))
  (define new-rows (remove (number->string user-id)
                           old-rows
                           (λ (user-id row) (string=? user-id (list-ref row 0)))))
  (define count (length new-rows))
  (when (not (= (length old-rows) count))
    (define out (open-output-file db-filename
                                  #:mode 'text
                                  #:exists 'replace))
    (if (> count 0)
        (for-each
         (λ (row) (write-string (string-append (string-join row ", ") "\n") out))
         new-rows)
        (write-string "" out))
    (close-output-port out)))


(define (remind-parse-args words)
  (match words
    [(or (list "提醒" "我" time-text content ...)
         (list "提醒" "我" time-text "的" "时候" content ...)
         (list "提醒" time-text content ...)
         (list time-text "提醒" "我" content ...))
     (cond
       [(regexp-match? #rx"^[0-9]*:?[0-9]*:?[0-9]*$" time-text)
        (define parts (map string->number (string-split time-text ":")))
        (and (> (length parts) 0))
        (define hour? (λ (v) (<= 0 v 23)))
        (define t60? (λ (v) (<= 0 v 59)))
        (define times (match parts
                        [(list (? hour? hour) (? t60? minute))
                         (list hour minute 0)]
                        [(list (? hour? hour) (? t60? minute) (? t60? second))
                         (list hour minute second)]
                        [_ #f]))
        (cond
          [times
           (match-define (list hour minute second) times)
           (define now (current-date))
           (define time
             (date->seconds (date second
                                  minute
                                  hour
                                  (date-day now)
                                  (date-month now)
                                  (date-year now)
                                  (date-week-day now)
                                  (date-year-day now)
                                  (date-dst? now)
                                  (date-time-zone-offset now))))
           (set! content (string-join content ""))
           (list time content)]
          [else #f])]
       [else #f])]
    [else #f]))