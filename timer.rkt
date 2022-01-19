#lang racket
(require racket/date
         (rename-in web-server/private/timer
                    [start-timer-manager start-timeout-manager]
                    [start-timer start-timeout]
                    [cancel-timer! cancel-timeout!]))

(provide timer%
         repeat-timer%
         set-timeout
         cancel-timeout!)


(define timeout-manager (start-timeout-manager))


(define (set-timeout callback seconds)
  (start-timeout timeout-manager seconds callback))


(define repeat-timer%
  (class object%
    (super-new)
    (init-field on-repeat times seconds [on-end #f])

    (define current 0)
    (define timeout #f)

    (define/public (start)
      (when (not timeout)
        (set! current 0)
        (set-timeout)))
    
    (define/public (cancel)
      (when timeout
        (cancel-timeout! timeout)
        (set! timeout #f)
        (set! current times)))

    (define (set-timeout)
      (set! timeout
            (start-timeout timeout-manager
                           seconds
                           handle-repeat)))

    (define (handle-repeat)
      (cond
        [(< current times)
         (set! current (+ current 1))
         (on-repeat current)
         (set-timeout)]
        [on-end (on-end)]))))


(define timer%
  (class object%
    (super-new)

    (init-field time on-timeout)

    (define timeout #f)

    (define/public (start)
      (when (not timeout)
        (define timeout-seconds (- time (date->seconds (current-date))))
        (define (timeout-callback)
          (set! timeout #f)
          (on-timeout))
        (if (<= timeout-seconds 0)
            (timeout-callback)
            (set! timeout (set-timeout timeout-callback timeout-seconds)))))

    (define/public (stop)
      (when timeout
        (cancel-timeout! timeout)
        (set! timeout #f)))))