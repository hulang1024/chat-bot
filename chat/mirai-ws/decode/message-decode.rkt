#lang racket
(require racket/match
         "../../message/message.rkt"
         "../../message/message-chain-builder.rkt")

(provide decode-message-chain)


(define (decode-message-chain js-array-expr)
  (define message-chain-builder (new message-chain-builder%))
  
  (for ([jsexpr js-array-expr])
    (define message (decode-message jsexpr))

    (send message-chain-builder add message))

  (send message-chain-builder build))

(define (decode-message jsexpr)
  (define type (hash-ref jsexpr 'type))
  (match type
    ["Source" (decode-source jsexpr)]
    ["Plain" (decode-plain jsexpr)]
    ["At" (decode-at jsexpr)]
    ["Quote" (decode-quote jsexpr)]
    [_ "?"]))

(define (decode-source jsexpr)
  (match-define(hash-table ('id id)
                           ('time time)) jsexpr)
  (new source%
       [id id]
       [time time]))

(define (decode-plain jsexpr)
  (match-define (hash-table ('text text)) jsexpr)
  (new plain% [text text]))

(define (decode-at jsexpr)
  (match-define (hash-table ('target target)
                            ('display display)) jsexpr)
  (new at%
       [target target]
       [display display]))

(define (decode-quote jsexpr)
  (match-define (hash-table ('id id)
                            ('groupId group-id)
                            ('senderId sender-id)
                            ('targetId target-id)
                            ('origin origin-jsexpr)) jsexpr)
  (define origin (decode-message origin-jsexpr))
  (new quote%
       [id id]
       [group-id group-id]
       [sender-id sender-id]
       [target-id target-id]
       [origin origin]))


(module+ test
  (require rackunit)
  (check-eq? (send (decode-message-chain null) empty?) #t)

  (define mc-jsexpr (list #hash((type . "Source") (id . 123) (time . 456))
                          #hash((type . "Plain") (text . "hi"))
                          #hash((type . "At") (target . "target") (display . "display"))))
  (define mc (decode-message-chain mc-jsexpr))
  (for-each (λ (m)
              (display (send m content-to-string)))
            (send mc get-list)))
    