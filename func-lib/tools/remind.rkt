#lang racket
(require racket/date
         "../../timer.rkt"
         "../../chat/message/main.rkt"
         "../../chat/contact/group.rkt")

(provide make-remind/time
         cancel-remind
         remind-time
         remind/time-parse-args-from-text)


(define user-remind-timers (make-hash))

(struct remind-time (hour minute second) #:transparent)

(define (format-remind-time time)
  (define sec (remind-time-second time))
  (string-append (format "~a:~a"
                         (remind-time-hour time)
                         (remind-time-minute time))
                 (if (> sec 0) (string-append ":" (number->string sec)) "")))

(define (make-remind/time subject user time thing add-message)
  (define (handle-timeout)
    (define mcb (new message-chain-builder%))
    (define add-message (create-add-message mcb))
    (when (is-a? subject group%)
      (add-message (new at% [target (send user get-id)])))
    (add-message (string-append " " thing))
    (send subject send-message (send mcb build)))
  
  (define now (current-date))
  (define timer-date
    (date (remind-time-second time)
          (remind-time-minute time)
          (remind-time-hour time)
          (date-day now)
          (date-month now)
          (date-year now)
          (date-week-day now)
          (date-year-day now)
          (date-dst? now)
          (date-time-zone-offset now)))

  (cond
    [(> (date->seconds timer-date) (date->seconds now))
     (define user-id (send user get-id))
     (cancel-timer user-id)
     (define timer (new timer%
                        [date timer-date]
                        [on-timeout handle-timeout]
                        [repeat 3]))
     (hash-set! user-remind-timers user-id timer)
     (send timer start)
     (add-message (face-from-id 178))
     (add-message (format "好的，将在~a的时候提醒你~a" (format-remind-time time) thing))]
    [else
     (add-message "时间已过")]))


(define (cancel-remind user add-message)
  (define user-id (send user get-id))
  (cond
    [(cancel-timer user-id)
     (add-message "已经取消提醒啦")]
    [else
     (add-message (face-from-id 32))
     (add-message "你什么时候设置的提醒")]))


(define (cancel-timer user-id)
  (cond
    [(hash-has-key? user-remind-timers user-id)
     (define timer (hash-ref user-remind-timers user-id))
     (send timer stop)
     (hash-remove! user-remind-timers user-id)
     #t]
    [else #f]))


(define (remind/time-parse-args-from-text text)
  (define re #rx"提醒我在([0-9]+[：:]?)+的时候.+")
  (cond
    [(regexp-match? re text)
     (define time-end-match (regexp-match-positions #rx"的时候" text))
     (cond
       [(not (null? time-end-match))
        (define time-end (caar time-end-match))
        (define time-text (substring text 4 time-end))
        (define parts (string-split time-text "："))
        (set! parts (map string->number
                         (flatten (map (λ (s) (string-split s ":")) parts))))
        (define hour? (λ (v) (<= 0 v 23)))
        (define t60? (λ (v) (<= 0 v 59)))
        (define time (match parts
                       [(list (? hour? hour) (? t60? minute))
                        (remind-time hour minute 0)]
                       [(list (? hour? hour) (? t60? minute) (? t60? second))
                        (remind-time hour minute second)]
                       [_ #f]))
        (define thing (string-trim (substring text (cdar time-end-match))))
        (list time thing)]
       [else #f])]
    [else #f]))