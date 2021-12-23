#lang racket
(require "message.rkt"
         "message-chain.rkt")

(provide message-chain-builder%
         make-quote-reply)


(define message-chain-builder%
  (class object%
    (super-new)

    (define lst null)

    (define/public (add m)
      (set! lst (append lst (cons m null))))

    (define/public (build)
      (define message-chain (new message-chain%))
      (for-each (λ (m)
                  (send message-chain add m))
                lst)
      message-chain)))


(define (make-quote-reply source-message)
  (cond
    [(is-a? source-message message-chain%)
     (define source (send source-message get source%))
     (define id (send source get-id))
     (new quote%
       [id id]
       [origin source-message])]))