#lang racket

(require "config.rkt"
         "chat/event/message-event.rkt"
         "chat/bot.rkt"
         "chat/message/main.rkt"
         "func-lib/cmdline-message-handler.rkt")


(displayln "Started")

(define bot (new bot%
                 [server-config mirai-ws-server-config]
                 [verbose #t]
                 [client-debug #t]))

(send bot subscribe-message-event
      (λ (event)
        (define subject (send event get-subject))
        (define mcb (new message-chain-builder%))
        (define add-message (create-add-message mcb))

        (define handled (handle-cmdline-message (send event get-message)
                                                (send event get-sender)
                                                add-message))
        (when handled
          (define mc (send mcb build))
          (when (not (send mc empty?))
            (send subject send-message (send mc trim))))))

(displayln "连接中...")
(send bot login
      (λ ()
        (displayln "已连接到服务器:)")))