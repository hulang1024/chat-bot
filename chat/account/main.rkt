#lang racket
(require json
         "../mirai-ws/command.rkt"
         "../mirai-ws/decode/account-decode.rkt")

(provide get-group-list)


(define (get-group-list bot)
  (define conn (send bot get-client-connection))
  (define promise (client-send-command! conn "groupList" #:log? (send bot verbose?)))
  (send promise then
        (λ (ret)
          (define data (hash-ref (send ret get-data) 'data))
          (map (λ (expr) (decode-group expr bot)) data))))
      