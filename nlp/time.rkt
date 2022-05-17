#lang racket
(require racket/date
         "tagging.rkt")

(provide time-word->date
         date->short-string
         date-seconds->short-string)


(define (time-word->date word [context null])
  (define base-date (current-date))

  (define (make-part-name-value)
    (make-hash
     (list (cons 'second (date-second base-date))
           (cons 'minute (date-minute base-date))
           (cons 'hour (date-hour base-date))
           (cons 'day (date-day base-date))
           (cons 'month (date-month base-date))
           (cons 'year (date-year base-date)))))
  
  (define exprs (if (tagged-word? word) (tagged-word-data word) word))

  (define (get-expr i)
    (if (< i (length exprs))
        (list-ref exprs i)
        #f))

  (define (mod-expr-ops new-op i)
    (let for-mod ([j (- i 1)])
      (when (>= j 0)
        (define expr (get-expr j))
        (when (and (time-expr? expr)
                   (equal? (time-expr-op expr) '=)
                   (not (false? (time-expr-duration? expr))))
          (set! exprs
                (list-set exprs j (time-expr new-op
                                             (time-expr-part-name expr)
                                             (time-expr-value expr)
                                             #t))))
        (for-mod (- j 1)))))

  (let loop ([i 0])
    (when (< i (length exprs))
      (define expr (get-expr i))
      (match expr
        ['after (mod-expr-ops '+ i)]
        ['before (mod-expr-ops '- i)]
        [(? hour-section? _)
         (set! exprs
               (list-set exprs i (time-expr '= 'hour (hour-section-start expr) #f)))]
        [else #t])
      (loop (+ i 1))))

  (define time-exprs (filter time-expr? exprs))

  (define part-name-value (make-part-name-value))

  (when (and (not (null? time-exprs))
             (false? (time-expr-duration? (last time-exprs))))
    ; 精确到什么时间分量
    (match (time-expr-part-name (last time-exprs))
      ['year
       (hash-set! part-name-value 'month 1)
       (hash-set! part-name-value 'day 1)
       (hash-set! part-name-value 'hour 0)
       (hash-set! part-name-value 'minute 0)
       (hash-set! part-name-value 'second 0)]
      ['month
       (hash-set! part-name-value 'day 1)
       (hash-set! part-name-value 'hour 0)
       (hash-set! part-name-value 'minute 0)
       (hash-set! part-name-value 'second 0)]
      ['day
       (hash-set! part-name-value 'hour 0)
       (hash-set! part-name-value 'minute 0)
       (hash-set! part-name-value 'second 0)]
      ['hour
       (hash-set! part-name-value 'minute 0)
       (hash-set! part-name-value 'second 0)]
      ['minute
       (hash-set! part-name-value 'second 0)]
      [else #t]))

  (define (part-name-value->date)
    (date (hash-ref part-name-value 'second)
          (hash-ref part-name-value 'minute)
          (hash-ref part-name-value 'hour)
          (hash-ref part-name-value 'day)
          (hash-ref part-name-value 'month)
          (hash-ref part-name-value 'year)
          (date-week-day base-date)
          (date-year-day base-date)
          #f
          0))
 
  (define (unit->seconds unit value)
    (match unit
      ['day (unit->seconds 'hour (* value 24))]
      ['hour (unit->seconds 'minute (* value 60))]
      ['minute (* value 60)]
      ['second value]
      [else 0]))

  (define (add-time part-name value)
    (define seconds (+ (date->seconds (part-name-value->date))
                       (unit->seconds part-name value)))
    (set! base-date (seconds->date seconds))
    (set! part-name-value (make-part-name-value)))

  (define ok? #t)
  (for-each
   (λ (expr)
     (define part-name (time-expr-part-name expr))
     (define base-value (hash-ref part-name-value part-name))
     (with-handlers ([(const #t) (λ (v) (set! ok? #f))])
       (match (time-expr-op expr)
         ['+ (add-time part-name (time-expr-value expr))]
         ['- (add-time part-name (- (time-expr-value expr)))]
         ['= (hash-set! part-name-value part-name
                        (time-expr-value expr))])))
   time-exprs)
  (and ok?
       (with-handlers ([(const #t) (λ (v) #f)])
         (part-name-value->date))))


(define (date-seconds->short-string time now)
  (date->short-string (seconds->date time) now))

(define (date->short-string date now)
  (if (today? date now)
      (format-time date)
      (string-append (format-day date) " " (format-time date))))

(define (format-day date)
  (format "~a-~a-~a"
          (date-year date)
          (leftpad-0 (date-month date))
          (leftpad-0 (date-day date))))

(define (format-time date)
  (format "~a:~a:~a"
          (leftpad-0 (date-hour date))
          (leftpad-0 (date-minute date))
          (leftpad-0 (date-second date))))

(define (today? date today)
  (and (= (date-year date) (date-year today))
       (= (date-year-day date) (date-year-day today))))

(define (leftpad-0 v [w 2])
  (~a v #:width 2 #:align 'right #:pad-string "0"))
      