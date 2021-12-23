#lang racket
(require racket/match
         json
         "../message/main.rkt"
         "../mirai-ws/client.rkt"
         "../mirai-ws/encode/message-encode.rkt")

(provide send-group-message
         send-friend-message)


(define (send-group-message #:group group
                            #:message message)
  (send-group-or-friend-message group message))
  

(define (send-friend-message #:friend friend
                             #:message message)
  (send-group-or-friend-message friend message))


(define (send-group-or-friend-message subject message)
  (define target (send subject get-id))
  (define message-chain (as-message-chain message))

  (define command (make-hash `((target . ,target)
                               (messageChain . ,(encode-message-chain message-chain)))))

  (define quote-m (send message-chain get quote%))
  (when quote-m
    (hash-set! command 'quote (send quote-m get-id)))
  
  (define type (if (equal? (object-name subject) 'object:group%)
                   'sendGroupMessage
                   'sendFriendMessage))
  
  (send-command (send subject get-bot) type command))


(define (send-command bot type content)
  (define command (make-hash `((command . ,(symbol->string type))
                               (subCommand . ,(json-null))
                               (content . ,content))))
  (define conn (send bot get-client-connection))
  (define command-json (client-send-command conn command))
  (when (send bot verbose?)
    (define command-json (jsexpr->string command))
    (printf "-> ~a\n" command-json)))
      