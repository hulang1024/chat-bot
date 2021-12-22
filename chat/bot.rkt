#lang racket

; 机器人

(require "mirai-ws/client.rkt"
         "mirai-ws/decode/message-event-decode.rkt")


(define bot%
  (class object%
    (super-new)

    (init-field server-config)

    (define client-conn #f)
    (define message-event-handler #f)

    (define/public (login)
      (set! client-conn
            (client-connect #:config server-config
                            #:on-message-channel-data on-message-channel-data)))

    (define/public (get-client-connection) client-conn)
    
    (define/public (subscribe-message-event proc)
      (set! message-event-handler proc))

    (define (on-message-channel-data data)
      (define message-event (decode-message-event data this))
      (when message-event
        (emit-message-event-handlers message-event)))

    (define (emit-message-event-handlers event)
      (when message-event-handler
        (message-event-handler event)))))
    