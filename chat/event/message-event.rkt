#lang racket

(provide message-event%
         group-message-event%
         friend-message-event%)


(define message-event%
  (class object%
    (super-new)

    (init-field bot message)

    (define/public (get-bot) bot)
    (define/public (get-message) message)
    
    (abstract get-subject
              get-sender)))


(define group-message-event%
  (class message-event%
    (super-new)

    (init-field group group-member)

    (define/override (get-subject) group)
    (define/override (get-sender) group-member)))


(define friend-message-event%
  (class message-event%
    (super-new)

    (init-field friend)

    (define/override (get-subject) friend)
    (define/override (get-sender) friend)))