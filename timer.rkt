#lang racket
(require racket/date
         (rename-in web-server/private/timer
                    [start-timer-manager start-timeout-manager]
                    [start-timer start-timeout]
                    [cancel-timer! cancel-timeout!]))

(provide timer%)


(define timeout-manager (start-timeout-manager))
(define timeout #f)
(define tickers null)

(define (tick)
  (set! timeout
        (start-timeout
         timeout-manager
         1
         (λ ()
           (define now (current-date))
           (for-each (λ (ticker) (ticker now)) tickers)
           (tick)))))


(define (add-ticker ticker)
  (set! tickers (append tickers (cons ticker null)))
  (when (not timeout)
    (thread (λ () (tick)))))

(define (remove-ticker ticker)
  (set! tickers (remove ticker tickers))
  (when (null? tickers)
    (cancel-timeout! timeout)))

(define timer%
  (class object%
    (super-new)

    (init-field date on-timeout [repeat 1])

    (define state 'ready)
    (define repeat-count 0)
    (define timeout-seconds (date->seconds date))
    
    (define/public (start)
      (when (equal? state 'ready)
        (set! state 'running)
        (set! repeat-count 0)
        (add-ticker ticker)))

    (define/public (stop)
      (set! state 'ready)
      (remove-ticker ticker))

    (define (ticker now)
      (when (> (date->seconds now) timeout-seconds)
        (on-timeout)
        (when (< repeat-count repeat)
          (set! repeat-count (+ repeat-count 1)))
        (when (= repeat-count repeat)
          (remove-ticker ticker))))))