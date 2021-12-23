#lang racket
(require "contact.rkt"
         "message-send.rkt")

(provide friend%)


(define friend%
  (class contact%
    (super-new)

    (init-field nickname remark)

    (define/public (get-nickname) nickname)
    (define/public (get-remark) remark)

    (define/override (send-message message)
      (send-friend-message #:friend this
                           #:message message))))