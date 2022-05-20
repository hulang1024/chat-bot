#lang racket
(require racket/date)

(provide (prefix-out consumer-mgr: use-func)
         (prefix-out consumer-mgr: query-times)
         (prefix-out consumer-mgr: query-to-now))

         
; 用户 -> 功能今日使用次数hash
(define user-func-times-hash (make-hash))
(define last-date (current-date))
; 用户 -> 功能最后使用时间hash
(define user-func-last-time-hash (make-hash))

(define (use-func func-id user)
  (define user-id (send user get-id))

  (define now (current-date))
  (when (or (> (date-year now) (date-year last-date))
            (> (date-year-day now) (date-year-day last-date)))
    (set! user-func-times-hash (make-hash))
    (set! last-date now))
  
  (define func-times-hash (hash-ref! user-func-times-hash user-id (make-hash)))
  (define times (hash-ref func-times-hash func-id 0))

  (hash-set! func-times-hash func-id (+ times 1))
  (define func-last-time-hash (hash-ref! user-func-last-time-hash user-id (make-hash)))
  (hash-set! func-last-time-hash func-id (current-inexact-milliseconds)))


(define (query-times func-id user)
  (define user-id (send user get-id))
  (if (hash-has-key? user-func-times-hash user-id)
      (hash-ref (hash-ref user-func-times-hash user-id) func-id 0)
      0))

(define (query-to-now func-id user)
  (define last-time (query-last-time func-id user))
  (and last-time (/ (max (- (current-inexact-milliseconds) last-time) 0) 1000)))

(define (query-last-time func-id user)
  (define user-id (send user get-id))
  (and (hash-has-key? user-func-last-time-hash user-id)
       (hash-ref (hash-ref user-func-last-time-hash user-id) func-id #f)))