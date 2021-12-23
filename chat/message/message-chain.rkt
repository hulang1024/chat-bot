#lang racket
(require "./message.rkt")

(provide message-chain%
         as-message-chain)


(define message-chain%
  (class* object% (message<%>)
    (super-new)

    (define lst null)

    (define/public (to-list) lst)

    (define/public (get p)
      (cond
        [(number? p) (list-ref lst p)]
        [(class? p) (get (is-a?/c p))]
        [(procedure? p)
         (findf p lst)]))

    (define/public (add m)
      (when (string? m)
        (set! m (new plain% [text m])))
      (set! lst (append lst (cons m null))))

    (define/public (empty?) (null? lst))

    (define/public (content-to-string) (void))))


(define (as-message-chain message)
  (cond
    [(is-a? message message-chain%)
      message]
    [(or (is-a? message single-message<%>) (string? message))
     (define c (new message-chain%))
     (send c add message)
     c]))