#lang racket
(require racket/date
         2htdp/batch-io)

(provide get-holiday-text
         get-today-text
         get-offwork-rest-seconds
         make-holiday-data
         get-holiday-value
         leftpad)


(define holiday-file-path (build-path "/home/chat-bot/data/holiday.csv"))
(define holidays #f)

(define (get-holiday-text add-message)
  (for-each
   (λ (item)
     (add-message (format "距离 ~a：~a天\n" (car item) (cadr item))))
   (sort (make-holiday-data) < #:key cadr)))

(define (make-holiday-data)
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

  `((本周末 ,(max 0 (- 5 (date-week-day now))))
    ,@(map (λ (h) (list h (get-holiday-rest-day h)))
           '(春节 清明节 劳动节 端午节 中秋节 国庆节 元旦))))


(define (get-holiday-value data)
  (λ (name) (cadr (assoc name data))))


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


(define (get-today-text)
  (define now (current-date))
  (format "~a月~a日周~a~a"
          (leftpad (date-month now) 2 "0")
          (leftpad (date-day now) 2 "0")
          (string-ref "日一二三四五六" (date-week-day now))
          (get-hour-section-name (date-hour now))))


(define (get-offwork-rest-seconds)
  (define now (current-date))
  (define offwork-time
    (find-seconds 0 0 18
                  (date-day now) (date-month now) (date-year now)))
  (max 0 (floor (/ (- offwork-time (date->seconds now)) 60))))


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