#lang racket
(require "./message.rkt")


(provide message-chain-builder%)


(define message-chain-builder%
  (class object%
    (super-new)

    (define lst null)

    (define/public (add m)
      (set! lst (append lst (cons m null))))

    (define/public (build)
      (define message-chain (new message-chain%))
      (for-each (λ (m) (send message-chain add m))
                lst)
      message-chain)))
      