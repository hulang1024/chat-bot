#lang racket

(require "../mirai-ws/decode/message-decode.rkt")

(provide send-friend-message
         send-group-message)


(define (send-friend-message #:bot bot #:target target #:message message [#:quote quote])
  (define req (hash 'target target
                    'messageChain (decode-message message)
                    'quote quote))
  (send-command bot 'SendFriendMessage req))

(define (send-group-message #:bot bot #:target target #:message message [#:quote quote])
  (define req (hash 'target target
                    'messageChain (decode-message message)))
  (send-command bot 'SendGroupMessage req))

(define (send-command bot type message-hash)
  (define command (make-hash `((command . ,type)
                               (subCommand . ,(json-null))
                               (content . ,message-hash))))
  (define command-json (jsexpr->string command))
  (define conn (send bot get-client-connection))
  (ws-send conn command-json))