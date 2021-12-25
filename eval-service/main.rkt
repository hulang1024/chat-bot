#lang racket
(require "lisp/handle-message.rkt")

(provide handle-eval-service-message)


(define (handle-eval-service-message event add-message)
  (handle-message event add-message))