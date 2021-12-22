#lang racket

(provide contact%)


(define contact%
  (class object%
    (super-new)

    (init-field bot
                id)

    (abstract send-message)))