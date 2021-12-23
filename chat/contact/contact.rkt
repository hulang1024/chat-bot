#lang racket

(provide contact%)


(define contact%
  (class object%
    (super-new)

    (init-field bot
                id)

    (define/public (get-id) id)
    (define/public (get-bot) bot)

    (abstract send-message)))