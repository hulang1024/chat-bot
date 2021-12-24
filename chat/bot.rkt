#lang racket
(require "mirai-ws/client.rkt"
         "mirai-ws/decode/message-event-decode.rkt")

(provide bot%)


(define bot%
  (class object%
    (super-new)

    (init-field server-config
                [verbose #f]
                [client-debug #f])

    (define client-conn #f)
    (define message-event-handler #f)

    (define/public (verbose?) verbose)

    (define/public (login on-ok)
      (set! client-conn
            (client-connect #:config server-config
                            #:debug-mode client-debug
                            #:on-connected (on-connected on-ok)
                            #:on-message-channel-data on-message-channel-data)))

    (define/public (get-client-connection) client-conn)
    
    (define/public (subscribe-message-event proc)
      (set! message-event-handler proc))

    (define (on-connected on-ok)
      (λ (conn)
        (set! client-conn conn)
        (on-ok)))

    (define (on-message-channel-data data)
      (define message-event (decode-message-event data this))
      (when message-event
        (when verbose
          (displayln message-event))
        (emit-message-event-handlers message-event)))

    (define (emit-message-event-handlers event)
      (message-event-handler event))))
    