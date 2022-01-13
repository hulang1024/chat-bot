#lang racket
(require "user-db.rkt")

(provide (prefix-out user-manager: add-or-update))


(define (add-or-update user)
  (define id (send user get-id))
  (define nickname (send user get-nickname))
  (user-repo:add-or-update (s-user id nickname #f)))