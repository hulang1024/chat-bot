#lang racket
(require "user.rkt"
         "message-send.rkt")

(provide friend%)


(define friend%
  (class user%
    (super-new)

    (init-field [nickname ""] [remark ""])

    (define/public (get-remark) remark)

    (define/override (get-nickname) nickname)
    (define/override (send-message message)
    (send-friend-message #:friend this
                          #:message message))))