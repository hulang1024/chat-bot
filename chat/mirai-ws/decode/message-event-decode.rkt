#lang racket

(require racket/match
         "../../event/message-event.rkt"
         "contact-decode.rkt"
         "message-decode.rkt")

(provide decode-message-event)


(define (decode-message-event jsexpr bot)
  (match-let jsexpr
    ([hash-table ('type type)
                 ('messageChain message-chain-jsexpr)
                 ('sender sender-jsexpr)])

    (define message-chain (decode-message-chain message-chain-jsexpr))
    
    (match type
      ["GroupMessage"
       (define group-member (decode-group-member sender-jsexpr bot))
       (group-message-event #:bot bot
                            #:message message-chain
                            #:sender group-member
                            #:subject (send group-member get-group))]
      ["FriendMessage"
       (define friend (decode-friend sender-jsexpr bot))
       (friend-message-event #:bot bot
                             #:message message-chain
                             #:sender friend
                             #:subject friend)]
      [_ #f])))

