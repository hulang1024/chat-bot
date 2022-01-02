#lang racket
(require racket/match
         "contact-decode.rkt"
         "message-decode.rkt"
         "../../event/message-event.rkt")

(provide decode-message-event)


(define (decode-message-event jsexpr bot)
  (match-define (hash-table ('type type)
                            ('messageChain message-chain-jsexpr)
                            ('sender sender-jsexpr)) jsexpr)

  (define message-chain (decode-message-chain message-chain-jsexpr))
    
  (match type
    ["GroupMessage"
     (define group-member (decode-group-member sender-jsexpr bot))
     (new group-message-event% [bot bot]
                               [message message-chain]
                               [group (send group-member get-group)]
                               [group-member group-member])]
    ["FriendMessage"
     (define friend (decode-friend sender-jsexpr bot))
     (new friend-message-event% [bot bot]
                                [message message-chain]
                                [friend friend])]
    [else #f]))