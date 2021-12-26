#lang racket
(require "tools/remind/main.rkt")

(provide handle-login)


(define (handle-login bot)
  (remind-load bot))