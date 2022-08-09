#lang racket
(require "../chat/contact/friend.rkt"
         "../chat/account/main.rkt"
         "../chat/message/main.rkt"
         "../config.rkt"
         "tools/remind/main.rkt")

(provide handle-login)


(define (handle-login bot notify-mode)
  (define mcb (new message-chain-builder%))
  (send mcb add (face-from-id 74))
  (case notify-mode
    [("admin")
      (send mcb add " 登录成功")
      (define msg (send mcb build))
      (for-each
        (λ (admin-id)
          (define friend (new friend% [bot bot] [id admin-id]))
          (send friend send-message msg))
        admin-ids)]
    [("all")
      (send (get-group-list bot) then
            (λ (groups)
              (send mcb add " 我来啦")
              (define msg (send mcb build))
              (for-each
                (λ (group)
                  (send group send-message msg))
                groups)))])
  (remind-load bot))