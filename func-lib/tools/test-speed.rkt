#lang racket
(require "../../chat/contact/message-send.rkt")

(provide test-speed)

(define (test-speed event)
  (define subject (send event get-subject))
  (define sent-time (current-inexact-milliseconds))
  (define receipt (send subject send-message "好的，请稍等"))
  (message-receipt-promise-then
   receipt
   (λ (_)
     (define usetime (round (- (current-inexact-milliseconds) sent-time)))
     (send subject send-message (format "从发送消息到收到消息回执用时 ~a ms" usetime)))))