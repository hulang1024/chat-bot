#lang racket

(require "contact.rkt"
         "../message/message-send.rkt")

(provide group%
         group-member%)


(define group%
  (class contact%
    (super-new)

    (inherit-field bot
                   id)

    (define/override (send-message message)
      (send-group-message #:bot bot
                          #:target id
                          #:message message))))

(define group-member%
  (class contact%
    (super-new)

    (inherit-field bot
                   id
                   group)

    (define/public (get-group) group)

    (define/override (send-message message)
      (error "暂未实现"))))