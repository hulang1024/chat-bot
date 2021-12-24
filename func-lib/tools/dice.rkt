#lang racket
(require math/base
         "../../chat/message/message.rkt")

(provide random-dice)


(define (random-dice add-message)
  (add-message (new dice-message% [value (random-integer 1 7)])))