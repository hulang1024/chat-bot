#lang racket
(require math/base)

(provide list-random-ref)


(define (list-random-ref lst)
  (list-ref lst (random-natural (length lst))))
