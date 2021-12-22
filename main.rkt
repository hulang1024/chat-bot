#lang racket

(require "config.rkt"
         "chat/bot.rkt"
         "chat/event/message-event.rkt")


(displayln "Started")

(define (on-message-event event)
  (define sender (message-event-sender event))
  (define subject (message-event-subject event))
  (define message-chain (message-event-message event))

  (displayln sender)
  (displayln subject)
  (displayln message-chain)

  (send subject send-message "hi1222"))


(define bot (new bot% [server-config mirai-ws-server-config]))
(send bot subscribe-message-event on-message-event)