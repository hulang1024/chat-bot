#lang racket

(require "config.rkt"
         "chat/event/message-event.rkt"
         "chat/bot.rkt"
         "chat/message/main.rkt")


(displayln "Started")

(define bot (new bot%
                 [server-config mirai-ws-server-config]
                 [verbose #t]))

(send bot subscribe-message-event
      (λ (event)
        (define subject (send event get-subject))
        (define mcb (new message-chain-builder%))
        ; (send mcb add (make-quote-reply (send event get-message)))
        (send mcb add "abc")
        (send mcb add (new dice-message% [value 6]))
        (send mcb add "hhh")

        (send subject send-message (send mcb build))))

(displayln "连接中...")
(send bot login
      (λ ()
        (displayln "已连接到服务器:)")))