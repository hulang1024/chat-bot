#lang racket

(require "contact.rkt"
         "user.rkt"
         "message-send.rkt")

(provide group%
         group-member%)


(define group%
  (class contact%
    (super-new)

    (init-field [name ""])

    (define/public (get-name) name)

    (define/override (send-message message)
    (send-group-message #:group this
                        #:message message))))


(define group-member%
  (class user%
    (super-new)

    (init-field group member-name special-title)
    
    (define/public (get-group) group)
    (define/public (get-member-name) member-name)
    (define/public (get-special-title) special-title)

    (define/override (get-nickname) member-name)
    (define/override (send-message message)
      (error "暂未实现"))))