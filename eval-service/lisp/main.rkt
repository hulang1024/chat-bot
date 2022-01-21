#lang racket
(require "handle-message.rkt"
         "config.rkt"
         "../../config.rkt")

(provide (all-from-out "handle-message.rkt")
         lisp-eval-server:restart)

(define (lisp-eval-server:restart sender-id add-message)
  (when (memq sender-id admin-ids)
    (define ok? (system restart-shell))
    (add-message (if ok? "命令成功，请稍等重启完成" "重启失败"))))